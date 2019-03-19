{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TemplateHaskell        #-}
module Lvca.Bidirectional where

import Data.Map.Merge.Lazy       as Map
import Control.Monad.Error.Class
import Control.Monad.Trans.Except
import Control.Applicative
import Control.Monad             (join)
import Control.Lens              (makeLenses, (^?), at, ix, view, (.~))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Class (lift)
import Data.Foldable             (for_)
import Data.Map                  (Map)
import qualified Data.Map        as Map
import qualified Data.Set        as Set
import Data.Monoid               (First(First, getFirst))
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

-- TODO: this is wrong wrt to variables:
-- * two unequal variables could be instantiated to the same term
-- * we should check for variables in the context
checkEq :: MonadCheck m => Term -> Term -> m ()
checkEq (Term t1 ts1) (Term t2 ts2)
  = if t1 == t2
    then join $ sequence_ <$>
           (pairWith checkEq' ts1 ts2 ?? "checkEq mismatched term lengths")
    else throwError $ "checkEq mismatched terms: " ++ Text.unpack t1 ++
           " vs " ++ Text.unpack t2
checkEq (Bound a) (Bound b) = if a == b then pure () else throwError $
  -- TODO: show names
  "unequal vars: " ++ show a ++ " vs " ++ show b
checkEq (Free a) (Free b) = if a == b then pure () else throwError $
  "unequal vars: " ++ show a ++ " vs " ++ show b
checkEq Bound{} Term{} = pure ()
checkEq Term{} Bound{} = pure ()

checkEq' :: MonadCheck m => ([Text], Term) -> ([Text], Term) -> m ()
checkEq' (_, t1) (_, t2) = checkEq t1 t2

-- | Match a pattern (on the left) with a term (containing no variables).
matchSchemaVars :: Term -> Term -> Maybe (Map Text Term)
matchSchemaVars (Free v) tm
  = pure $ Map.singleton v tm
matchSchemaVars (Term head1 args1) (Term head2 args2)
  | head1 == head2
  && length args1 == length args2
  = Map.unions <$> traverse (uncurry matchSchemaVars') (zip args1 args2)
matchSchemaVars _ _
  = Nothing

matchSchemaVars' :: ([Text], Term) -> ([Text], Term) -> Maybe (Map Text Term)
matchSchemaVars' (_, t1) (_, t2) = matchSchemaVars t1 t2

instantiate :: MonadCheck m => Map Text Term -> Term -> m Term
instantiate env (Term tag subtms)
  = Term tag <$> traverse (instantiate' env) subtms
instantiate env (Free v)
  -- TODO show name
  = env ^? ix v ?? "instantiate couldn't find variable " ++ show v
instantiate env v@Bound{} = pure v

instantiate'
  :: MonadCheck m => Map Text Term -> ([Text], Term) -> m ([Text], Term)
instantiate' env (names, tm) = (names,) <$> instantiate (keyUpdate env) tm
  where keyUpdate m = Map.withoutKeys m (Set.fromList names)
  -- where keyUpdate m
  --         = Map Text.mapKeys (\x -> x - length names)
  --         $ Map Text.withoutKeys m (IntSet.fromList [0.. length names - 1])

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
  Env{_rules} <- ask

  let matchRule = \case
        Rule _ InferenceRule{} -> Nothing
        Rule hyps (CheckingRule (ruleTm :<= ruleTy)) -> do

          -- Find the terms which correspond to schema varas in the typing
          -- rule.  If the term and type in the focus of this rule match, then
          -- the rule matches.
          tmAssignments <- matchSchemaVars ruleTm tm
          tyAssignments <- matchSchemaVars ruleTy ty

          Just $ flip evalStateT Map.empty $
            for_ hyps $ \(patternCtx, rule) -> do
              ctxState <- get
              ctx' <- mergeCtx ctxState patternCtx ?? ""
              ctx <- traverse (instantiate tyAssignments) ctx'

              case rule of
                CheckingRule (hypTm :<= hypTy) -> do
                  -- TODO: should be (tmAssignments <> tyAssignments) /
                  -- (tmAssignments <> tyAssignments <> ctx)?
                  tm'  <- instantiate tmAssignments hypTm
                  ty'  <- instantiate tyAssignments hypTy
                  localCtx ctx $ check $ tm' :< ty'
                InferenceRule (hypTm :=> hypTy) -> do
                  -- TODO: see above
                  tm'  <- instantiate tmAssignments hypTm
                  ty'  <- instantiate tyAssignments hypTy
                  ty'' <- localCtx ctx $ infer tm'

                  -- Update the binding for any type variable we've infered
                  learnedTys <- matchSchemaVars hypTy ty''
                    ?? "check InferenceRule: failed context merge"
                  updateCtx learnedTys

                  checkEq ty' ty''

  case getFirst $ foldMap (First . matchRule) _rules of
    Just rule -> rule
    Nothing   -> throwError $ "No matching checking rule found for " ++ show tm

infer :: Term -> Check Term
infer tm = do
  Env{_rules} <- ask

  let matchRule = \case
        Rule _ CheckingRule{} -> Nothing
        Rule hyps (InferenceRule (ruleTm :=> ruleTy)) -> do

          -- Find the terms which correspond to schema vars in the typing rule.
          -- In contrast to checking, we match only the term in focus.
          assignments <- matchSchemaVars ruleTm tm

          pure $ flip evalStateT Map.empty $ do
            for_ hyps $ \(patternCtx, rule) -> do
              ctxState <- get
              let ctx = ctxState <> patternCtx

              case rule of
                CheckingRule  (hypTm :<= hypTy) -> do
                  ctx' <- mergeCtx assignments ctx ?? "bad merge"
                  tm' <- instantiate ctx' hypTm
                  ty' <- instantiate ctx' hypTy
                  localCtx ctx' $ check $ tm' :< ty'

                InferenceRule (hypTm :=> hypTy) -> do
                  -- TODO: should be (assignments <> ctx)?
                  tm' <- instantiate assignments hypTm
                  ty' <- localCtx ctx $ infer tm'

                  -- Update the binding for any type variable we've infered
                  learnedTys <- matchSchemaVars hypTy ty'
                    ?? "infer InferenceRule: failed context merge"
                  updateCtx learnedTys

                  checkEq hypTy ty'

            ctxState <- get
            ctxState' <- mergeCtx assignments ctxState ?? "TODO"
            instantiate ctxState' ruleTy

  case getFirst $ foldMap (First . matchRule) _rules of
    Just rule -> rule
    Nothing   -> ctxInfer tm

ctxInfer :: Term -> Check Term
ctxInfer = \case
  Free v -> view (varTypes . at v) ???
    -- TODO: show name
    ("Variable not found in context: " ++ show v)
  tm    -> throwError $
    "found no matching inference rule for this term: " ++ show tm


-- t2: Term "app" [([],Term "annot" [([],Term "lam" [(["x"],Var "x")]),([],Term "arr" [([],Term "Bool" []),([],Term "Bool" [])])]),([],Term "true" [])]

-- t2: Term "true" []

