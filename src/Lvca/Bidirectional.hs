{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TemplateHaskell        #-}
module Lvca.Bidirectional where

import Data.Map.Merge.Strict       as Map
import Control.Monad.Error.Class
import Control.Monad.Trans.Except
import Control.Applicative
import Control.Lens              (makeLenses, (^?), at, ix, view, (.~))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Class (lift)
import Data.Foldable             (for_)
import Data.Map.Strict                  (Map)
import qualified Data.Map.Strict        as Map
import qualified Data.Set        as Set
import Data.Monoid               (First(First, getFirst))
import Data.Text                 (Text)
import qualified Data.Text       as Text
import Data.Traversable          (for)

import Lvca.Util

-- Note: order matters (!), both the ordering of a set of rules and the
-- ordering of hypotheses within a rule.
--
-- * Write the rules you want to match first
-- * Write the hypotheses which produce information to be used in subsequent
--   hyps first.
--
-- Bidirectional typechecking is, after all, a strategy for algorithmization.

data Scope = Scope ![Text] !Term
  deriving (Eq, Show)

data Term
  = Term !Text ![Scope]
  | Bound !Int
  | Free !Text
  deriving (Eq, Show)

type PatternTerm = Term

-- | Inference rule
data InferenceRule = Term :=> Term
  deriving Show

-- | Checking rule
data CheckingRule = Term :<= Term
  deriving Show

data TypingClause
  = InferenceRule !InferenceRule
  | CheckingRule  !CheckingRule
  deriving Show

data Rule = Rule
  { _hypotheses :: ![(Map Text Term, TypingClause)]
  , _name       :: !(Maybe Text)
  , _conclusion :: !TypingClause
  }

data Env = Env
  { _rules    :: ![Rule]
  , _varTypes :: !(Map Text Term)
  }

makeLenses ''Env

type Check a = ReaderT Env (Except String) a

runCheck :: Env -> Check a -> Either String a
runCheck env c = runExcept $ runReaderT c env

data Typing = Term :< Term

type MonadCheck m = (Alternative m, MonadError String m)

open :: [Term] -> Scope -> Maybe Term
open args (Scope names body)
  | length args /= length names
  = Nothing
  | otherwise
  = open' 0 body
  where open' offset tm = case tm of
          Term tag subtms
            -> fmap (Term tag) $
              for subtms $ \(Scope binders subtm)
                  -> Scope binders <$> open' (offset + length binders) subtm
          Bound i
            | i >= offset
            -> args ^? ix (i - offset)
            | otherwise
            -> Just tm
          Free{}
            -> Just tm

instantiate :: MonadCheck m => Map Text Scope -> Term -> m Term
instantiate env tm = case tm of
  Term tag subtms
    -> fmap (Term tag) $ for subtms $ \(Scope names body) ->
      Scope names <$> instantiate (Map.withoutKeys env (Set.fromList names)) body
  Bound{}
    -> pure tm
  Free v -> do
    sc@(Scope names _) <- env ^? ix v
      ?? "Couldn't find scope in context: " ++ Text.unpack v
    open (fmap Free names) sc
      ?? "Failed to open scope: " ++ show sc

-- | Match a pattern (on the left) with a term (containing no variables).
matchSchemaVars :: Term -> Term -> Maybe (Map Text Scope)
matchSchemaVars (Free v) tm
  = pure $ Map.singleton v $ Scope [] tm
matchSchemaVars (Term tag1 args1) (Term tag2 args2)
  | tag1 == tag2
  && length args1 == length args2
  = Map.unions <$> zipWithA matchSchemaVars' args1 args2
  where matchSchemaVars' :: Scope -> Scope -> Maybe (Map Text Scope)
        matchSchemaVars' (Scope names1 body1) (Scope names2 body2)
          | length names1 /= length names2
          = Nothing
          | otherwise
          = fmap (enscope names1) <$> matchSchemaVars body1 body2
matchSchemaVars _ _
  = Nothing

enscope :: [Text] -> Scope -> Scope
enscope binders (Scope binders' body) = Scope (binders' <> binders) body

safeMerge
  :: (MonadCheck m, Eq a, Show a) => Map Text a -> Map Text a -> m (Map Text a)
safeMerge m1 m2 = Map.mergeA
  Map.preserveMissing
  Map.preserveMissing
  (Map.zipWithMaybeAMatched $ \_ x y
    -> if x == y then Just (Just x) else Nothing)
  m1
  m2
  ?? "failed merge of (" ++ show m1 ++ ") / (" ++ show m2 ++ ")"

-- Merge in the types of any terms we've just learned
updateCtx
  :: Map Text Scope
  -> StateT (Map Text Scope) (ReaderT Env (Except String)) ()
updateCtx ctx = do
  stateCtx  <- get
  stateCtx' <- safeMerge ctx stateCtx
  put stateCtx'

check :: Typing -> Check ()
check (tm :< ty) = do
  let matchRule = \case
        Rule _ _ InferenceRule{} -> Nothing
        Rule hyps _ (CheckingRule (ruleTm :<= ruleTy)) -> do

          -- Find the terms which correspond to schema varas in the typing
          -- rule.  If the term and type in the focus of this rule match, then
          -- the rule matches.
          tmAssignments <- matchSchemaVars ruleTm tm
          tyAssignments <- matchSchemaVars ruleTy ty

          Just $ flip evalStateT Map.empty $ do
            vts <- view varTypes

            ctxState     <- get
            assignments  <- safeMerge tmAssignments tyAssignments
            assignments' <- safeMerge ctxState assignments

            for_ hyps $ \(patternCtx, rule) -> do
              patternCtx' <- traverse (instantiate assignments') patternCtx
              localCtx    <- safeMerge vts patternCtx'

              case rule of
                CheckingRule (hypTm :<= hypTy) -> do
                  tm'  <- instantiate assignments' hypTm
                  ty'  <- instantiate assignments' hypTy
                  lift $ local (varTypes .~ localCtx) $ check $ tm' :< ty'

                InferenceRule (hypTm :=> hypTy) -> do
                  tm' <- instantiate assignments' hypTm
                  ty' <- lift $ local (varTypes .~ localCtx) $ infer tm'

                  -- Update the binding for any type variable we've infered
                  learnedTys <- matchSchemaVars hypTy ty'
                    ?? "check InferenceRule: failed to match schema vars"
                  updateCtx learnedTys

  Env{_rules} <- ask
  case getFirst $ foldMap (First . matchRule) _rules of
    Just rule -> rule
    Nothing   -> throwError $ "No matching checking rule found for " ++ show tm

infer :: Term -> Check Term
infer tm = do
  let matchRule = \case
        Rule _ _ CheckingRule{} -> Nothing
        Rule hyps _ (InferenceRule (ruleTm :=> ruleTy)) -> do

          -- Find the terms which correspond to schema vars in the typing rule.
          -- In contrast to checking, we match only the term in focus.
          assignments <- matchSchemaVars ruleTm tm

          pure $ flip evalStateT Map.empty $ do
            vts <- view varTypes

            for_ hyps $ \(patternCtx, rule) -> do
              ctxState     <- get
              assignments' <- safeMerge ctxState assignments

              patternCtx' <- traverse (instantiate assignments') patternCtx
              localCtx    <- safeMerge vts patternCtx'

              case rule of
                CheckingRule  (hypTm :<= hypTy) -> do
                  tm' <- instantiate assignments' hypTm
                  ty' <- instantiate assignments' hypTy
                  lift $ local (varTypes .~ localCtx) $ check $ tm' :< ty'

                InferenceRule (hypTm :=> hypTy) -> do
                  tm' <- instantiate assignments' hypTm
                  ty' <- lift $ local (varTypes .~ localCtx) $ infer tm'

                  -- Update the binding for any type variable we've infered
                  learnedTys <- matchSchemaVars hypTy ty'
                    ?? "infer InferenceRule: failed to match schema vars"
                  updateCtx learnedTys

            ctxState  <- get
            ctxState' <- safeMerge assignments ctxState
            instantiate ctxState' ruleTy

  Env{_rules} <- ask
  case getFirst $ foldMap (First . matchRule) _rules of
    Just rule -> rule
    Nothing   -> ctxInfer tm

ctxInfer :: Term -> Check Term
ctxInfer = \case
  Free v -> view (varTypes . at v)
    -- TODO: show name
    ??? "Variable not found in context: " ++ show v
  tm     -> throwError $
    "found no matching inference rule for this term: " ++ show tm
