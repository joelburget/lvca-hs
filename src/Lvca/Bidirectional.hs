{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TemplateHaskell        #-}

{-# LANGUAGE ScopedTypeVariables        #-}
module Lvca.Bidirectional where

import Data.Map.Merge.Strict       as Map
import Control.Monad.Error.Class
import Control.Monad.Trans.Except
import Control.Applicative
import Control.Monad             (join)
import Control.Lens              (makeLenses, (^?), at, ix, view, (.~), (.=), (<&>))
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
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Traversable (for)

import Lvca.Util

import Debug.Trace

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

type LocalCtx = Map Text Term

data TypingClause
  = InferenceRule !InferenceRule
  | CheckingRule  !CheckingRule
  deriving Show

data Rule = Rule
  { _hypotheses :: ![(LocalCtx, TypingClause)]
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
  = Just $ subst (Map.fromList $ zip names args) body

subst :: Map Text Term -> Term -> Term
subst m = \case
  Term name subtms -> Term name $ subtms <&> \(Scope names body)
    -> Scope names $ subst (Map.withoutKeys m (Set.fromList names)) body
  v@(Free name) -> case Map.lookup name m of
    Nothing -> v
    Just tm -> tm
  bound -> bound

instantiate :: MonadCheck m => Map Text Term -> Term -> m Term
instantiate env (Term tag subtms)
  = fmap (Term tag) $ for subtms $ \(Scope names body)
    -> Scope names <$> instantiate (Map.withoutKeys env (Set.fromList names)) body
instantiate env (Free v)
  -- TODO show name
  = env ^? ix v ?? "instantiate couldn't find variable " ++ show v
instantiate _ v@Bound{}
  = pure v

containsFree :: Text -> Term -> Bool
containsFree name = \case
  Free name'    -> name == name'
  Bound{}       -> False
  Term _ scopes -> any (\(Scope _ body) -> containsFree name body) scopes

-- TODO: this is wrong wrt to variables:
-- * two unequal variables could be instantiated to the same term
-- * we should check for variables in the context
checkEq :: forall m. MonadCheck m => Term -> Term -> m ()
checkEq (Term t1 ts1) (Term t2 ts2)
  = if t1 == t2
    then join $ fmap sequence_ $
           pairWith checkEq' ts1 ts2 ?? "checkEq mismatched term lengths"
    else throwError $ "checkEq mismatched terms: " ++ Text.unpack t1 ++
           " vs " ++ Text.unpack t2
    where checkEq' s1@(Scope names1 _) s2@(Scope names2 _)
            | length names1 /= length names2
            = throwError "TODO"
            | otherwise
            = do
                  -- find first available name for each variable we're
                  -- opening
              let freeVars = Free <$> findFreeScopeNames s1 s2
              s1' <- open freeVars s1 ?? "failed to open first scope"
              s2' <- open freeVars s2 ?? "failed to open second scope"
              checkEq s1' s2'
checkEq (Bound a) (Bound b) = if a == b then pure () else throwError $
  -- TODO: show names
  "unequal bound vars: " ++ show a ++ " vs " ++ show b
checkEq (Free a) (Free b) = if a == b then pure () else throwError $
  "unequal free vars: " ++ show a ++ " vs " ++ show b
checkEq Free{} Term{} = pure ()
checkEq Term{} Free{} = pure ()
checkEq a b
  = throwError $ "checkEq mismatched terms: " ++ show a ++ " / " ++ show b

findFreeScopeNames :: Scope -> Scope -> [Text]
findFreeScopeNames (Scope names1 body1) (Scope _ body2)
  = names1 <&> \name -> fromJust $ find
      (\name' -> not (containsFree name' body1)
              && not (containsFree name' body2))
      (name : ([1 :: Int ..] <&> \i -> name <> tShow i))

-- | Match a pattern (on the left) with a term (containing no variables).
matchSchemaVars :: Term -> Term -> Maybe (Map Text Term)
matchSchemaVars (Free v) tm
  = pure $ Map.singleton v tm
matchSchemaVars (Term head1 args1) (Term head2 args2)
  | head1 == head2
  && length args1 == length args2
  = Map.unions <$> zipWithA matchSchemaVars' args1 args2
    where matchSchemaVars' :: Scope -> Scope -> Maybe (Map Text Term)
          matchSchemaVars' s1@(Scope names1 _) s2@(Scope names2 _)
            | length names1 /= length names2
            = Nothing
            | otherwise
            = do
              let freeVars = Free <$> findFreeScopeNames s1 s2
              s1' <- open freeVars s1
              s2' <- open freeVars s2
              matchSchemaVars s1' s2'
          zipWithA :: Applicative p => (a -> b -> p c) -> [a] -> [b] -> p [c]
          zipWithA f x y = sequenceA (zipWith f x y)
matchSchemaVars _ _
  = Nothing

localCtx
  :: Map Text Term
  -> Check a
  -> StateT (Map Text Term) (ReaderT Env (Except String)) a
localCtx ctx action = do
  envCtx  <- view varTypes
  envCtx' <- mergeCtx envCtx ctx
    ?? "localCtx: failed context merge: " ++ show envCtx ++ " / " ++ show ctx
  lift $ local (varTypes .~ envCtx') action

mergeCtx :: Map Text Term -> Map Text Term -> Maybe (Map Text Term)
mergeCtx = Map.mergeA
  Map.preserveMissing
  Map.preserveMissing
  (Map.zipWithMaybeAMatched $ \_ x y -> if x == y then Just (Just x) else Nothing)

updateCtx
  :: Map Text Term
  -> StateT (Map Text Term) (ReaderT Env (Except String)) ()
updateCtx ctx = do
  stateCtx <- get
  stateCtx' <- mergeCtx ctx stateCtx ?? "updateCtx: failed context merge"
  put stateCtx'

check :: Typing -> Check ()
check (tm :< ty) = do
  let matchRule = \case
        Rule _ InferenceRule{} -> Nothing
        Rule hyps (CheckingRule (ruleTm :<= ruleTy)) -> do

          -- Find the terms which correspond to schema varas in the typing
          -- rule.  If the term and type in the focus of this rule match, then
          -- the rule matches.
          tmAssignments <- matchSchemaVars ruleTm tm
          tyAssignments <- matchSchemaVars ruleTy ty

          traceM $ "tmAssignments: " ++ show tmAssignments
          traceM $ "tyAssignments: " ++ show tyAssignments

          Just $ flip evalStateT Map.empty $ do
            assignments <- mergeCtx tmAssignments tyAssignments
              ?? "check failed to merge contexts"
            id .= assignments

            for_ hyps $ \(patternCtx, rule) -> do
              ctxState <- get
              ctx      <- mergeCtx ctxState patternCtx ?? "TODO"
              -- ctx      <- traverse (instantiate assignments) ctx'

              case rule of
                CheckingRule (hypTm :<= hypTy) -> do
                  tm'  <- instantiate ctx hypTm
                  ty'  <- instantiate ctx hypTy
                  localCtx ctx $ check $ tm' :< ty'

                InferenceRule (hypTm :=> hypTy) -> do
                  -- TODO: see above
                  tm'  <- instantiate ctx hypTm
                  ty'  <- instantiate ctx hypTy
                  ty'' <- localCtx ctx $ infer tm'

                  -- Update the binding for any type variable we've infered
                  learnedTys <- matchSchemaVars hypTy ty''
                    ?? "check InferenceRule: failed context merge"
                  updateCtx learnedTys

                  checkEq ty' ty''

  Env{_rules} <- ask
  case getFirst $ foldMap (First . matchRule) _rules of
    Just rule -> rule
    Nothing   -> throwError $ "No matching checking rule found for " ++ show tm

infer :: Term -> Check Term
infer tm = do
  let matchRule = \case
        Rule _ CheckingRule{} -> Nothing
        Rule hyps (InferenceRule (ruleTm :=> ruleTy)) -> do

          -- Find the terms which correspond to schema vars in the typing rule.
          -- In contrast to checking, we match only the term in focus.
          assignments <- matchSchemaVars ruleTm tm

          pure $ flip evalStateT assignments $ do
            for_ hyps $ \(patternCtx, rule) -> do
              ctxState <- get
              ctx      <- mergeCtx ctxState patternCtx ?? "TODO"

              case rule of
                CheckingRule  (hypTm :<= hypTy) -> do
                  tm' <- instantiate ctx hypTm
                  ty' <- instantiate ctx hypTy
                  localCtx ctx $ check $ tm' :< ty'

                InferenceRule (hypTm :=> hypTy) -> do
                  tm'  <- instantiate ctx hypTm
                  ty'  <- instantiate ctx hypTy
                  ty'' <- localCtx ctx $ infer tm'

                  -- Update the binding for any type variable we've infered
                  learnedTys <- matchSchemaVars hypTy ty''
                    ?? "infer InferenceRule: failed context merge"
                  updateCtx learnedTys

                  checkEq ty' ty''

            ctxState <- get
            ctxState' <- mergeCtx assignments ctxState ?? "TODO"
            instantiate ctxState' ruleTy

  Env{_rules} <- ask
  case getFirst $ foldMap (First . matchRule) _rules of
    Just rule -> rule
    Nothing   -> ctxInfer tm

ctxInfer :: Term -> Check Term
ctxInfer = \case
  Free v -> view (varTypes . at v) ???
    -- TODO: show name
    "Variable not found in context: " ++ show v
  tm     -> throwError $
    "found no matching inference rule for this term: " ++ show tm


-- t2: Term "app" [([],Term "annot" [([],Term "lam" [(["x"],Var "x")]),([],Term "arr" [([],Term "Bool" []),([],Term "Bool" [])])]),([],Term "true" [])]

-- t2: Term "true" []

