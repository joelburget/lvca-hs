{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TemplateHaskell        #-}
module Lvca.Bidirectional where

import Data.Map.Merge.Lazy       as Map
import Control.Monad.Error.Class
import Control.Monad.Trans.Except
import Control.Applicative
import Control.Monad             (join)
import Control.Lens              (makeLenses, (^?), at, ix, (%~), (%=), view)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Class (lift)
import Data.Foldable             (for_)
import Data.Map                  (Map)
import qualified Data.Map        as Map
import Data.Monoid               (First(First, getFirst))
import qualified Data.Set        as Set
import Data.Text                 (Text)
import qualified Data.Text       as Text

import Lvca.Util

-- Note: order matters (!), both the ordering of a set of rules and the
-- ordering of hypotheses within a rule.
--
-- * Write the rules you want to match first
-- * Write the hypotheses which produce information to be used in subsequent
--   hyps first.
--
-- Bidirectional typechecking is, after all, a strategy for algorithmization.

data Term
  = Term !Text ![([Text], Term)]
  | Var !Text
  deriving (Eq, Show)

type PatternTerm = Term

-- | Inference rule
data InferenceRule = Term :=> Term
  deriving Show

-- | Checking rule
data CheckingRule = Term :<= Term
  deriving Show

type Ctx = Map Text Term

data TypingClause
  = InferenceRule !InferenceRule
  | CheckingRule  !CheckingRule
  deriving Show

data Rule = Rule
  { _hypotheses :: ![(Ctx, TypingClause)]
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

checkEq :: MonadCheck m => Term -> Term -> m ()
checkEq (Term t1 ts1) (Term t2 ts2)
  = if t1 == t2
    then join $ sequence_ <$>
           (pairWith checkEq' ts1 ts2 ?? "checkEq mismatched term lengths")
    else throwError $ "checkEq mismatched terms: " ++ Text.unpack t1 ++
           " vs " ++ Text.unpack t2
checkEq (Var a) (Var b) = if a == b then pure () else throwError $
  "unequal vars: " ++ Text.unpack a ++ " vs " ++ Text.unpack b
checkEq Var{} Term{} = pure ()
checkEq Term{} Var{} = pure ()

-- XXX renaming
checkEq' :: MonadCheck m => ([Text], Term) -> ([Text], Term) -> m ()
checkEq' (_, t1) (_, t2) = checkEq t1 t2

-- | Match a pattern (on the left) with a term (containing no variables).
matchPatternVars :: Term -> Term -> Maybe (Map Text Term)
matchPatternVars (Var v) tm
  = pure $ Map.singleton v tm
matchPatternVars (Term head1 args1) (Term head2 args2)
  | head1 == head2
  && length args1 == length args2
  = Map.unions <$> traverse (uncurry matchPatternVars') (zip args1 args2)
matchPatternVars _ _
  = Nothing

-- XXX renaming
matchPatternVars' :: ([Text], Term) -> ([Text], Term) -> Maybe (Map Text Term)
matchPatternVars' (_, t1) (_, t2) = matchPatternVars t1 t2

instantiate :: MonadCheck m => Map Text Term -> Term -> m Term
instantiate env (Term tag subtms)
  = Term tag <$> traverse (instantiate' env) subtms
instantiate env (Var v)
  = env ^? ix v ?? ("instantiate couldn't find variable " ++ Text.unpack v)

instantiate'
  :: MonadCheck m => Map Text Term -> ([Text], Term) -> m ([Text], Term)
instantiate' env (names, tm)
  = (names,) <$> instantiate (Map.withoutKeys env (Set.fromList names)) tm

check :: Typing -> Check ()
check (tm :< ty) = do
  Env{_rules} <- ask

  let matchRule = \case
        Rule _ InferenceRule{} -> Nothing
        Rule hyps (CheckingRule (ruleTm :<= ruleTy)) -> do

          -- Find the terms which correspond to metavars in the typing rule.
          -- If the term and type in the focus of this rule match, then the
          -- rule matches.
          tmAssignments <- matchPatternVars ruleTm tm
          tyAssignments <- matchPatternVars ruleTy ty

          Just $ flip evalStateT Map.empty $
            for_ hyps $ \(patternCtx, rule) -> do
              ctxState <- get
              ctx <- traverse (instantiate tyAssignments) (mergeCtx ctxState patternCtx)

              case rule of
                CheckingRule (hypTm :<= hypTy) -> do
                  -- TODO: should be (tmAssignments <> tyAssignments) /
                  -- (tmAssignments <> tyAssignments <> ctx)?
                  tm'  <- instantiate tmAssignments hypTm
                  ty'  <- instantiate tyAssignments hypTy
                  lift $ local (varTypes %~ mergeCtx ctx) $ check $ tm' :< ty'
                InferenceRule (hypTm :=> hypTy) -> do
                  -- TODO: see above
                  tm'  <- instantiate tmAssignments hypTm
                  ty'  <- instantiate tyAssignments hypTy
                  ty'' <- lift $ local (varTypes %~ mergeCtx ctx) $ infer tm'

                  -- Update the binding for any type variable we've infered
                  learnedTys <- matchPatternVars hypTy ty'' ?? "TODO: msg"
                  id %= mergeCtx learnedTys

                  checkEq ty' ty''

  case getFirst $ foldMap (First . matchRule) _rules of
    Just rule -> rule
    Nothing   -> throwError $ "No matching checking rule found for " ++ show tm

mergeCtx :: Map Text Term -> Map Text Term -> Maybe (Map Text Term)
mergeCtx = Map.mergeA
  preserveMissing
  preserveMissing
  (zipWithMaybeAMatched $ \_ x y -> if x == y then Just (Just x) else Nothing)

infer :: Term -> Check Term
infer tm = do
  Env {_rules} <- ask

  let matchRule = \case
        Rule _ CheckingRule{} -> Nothing
        Rule hyps (InferenceRule (ruleTm :=> ruleTy)) -> do

          -- Find the terms which correspond to metavars in the typing rule.
          -- In contrast to checking, we match only the term in focus.
          assignments <- matchPatternVars ruleTm tm

          pure $ flip evalStateT Map.empty $ do
            for_ hyps $ \(patternCtx, rule) -> do
              ctxState <- get
              let ctx = ctxState <> patternCtx

              case rule of
                CheckingRule  (hypTm :<= hypTy) -> do
                  ctx' <- mergeCtx assignments ctx ?? "bad merge"
                  tm' <- instantiate ctx' hypTm
                  ty' <- instantiate ctx' hypTy
                  lift $ local (varTypes %~ mergeCtx ctx') $ check $ tm' :< ty'

                InferenceRule (hypTm :=> hypTy) -> do
                  -- TODO: should be (assignments <> ctx)?
                  tm' <- instantiate assignments hypTm
                  ty' <- lift $ local (varTypes %~ mergeCtx ctx) $ infer tm'

                  -- Update the binding for any type variable we've infered
                  learnedTys <- matchPatternVars hypTy ty' ?? "TODO: msg"
                  id %= mergeCtx learnedTys

                  checkEq hypTy ty'

            ctxState <- get
            instantiate (mergeCtx assignments ctxState) ruleTy

  case getFirst $ foldMap (First . matchRule) _rules of
    Just rule -> rule
    Nothing   -> ctxInfer tm

ctxInfer :: Term -> Check Term
ctxInfer = \case
  Var v -> view (varTypes . at v) ???
    ("Variable not found in context: " ++ Text.unpack v)
  tm    -> throwError $
    "found no matching inference rule for this term: " ++ show tm
