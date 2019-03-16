{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TemplateHaskell        #-}
module Lvca.Bidirectional where

import Data.Foldable             (for_)
import Control.Monad             (join)
import Data.Monoid               (First(First, getFirst))
import Data.Text                 (Text)
import Control.Lens              (makeLenses, (^?), ix, (<>~))
import Control.Monad.Reader
import Control.Monad.Trans.Class (lift)
import Data.Map                  (Map)
import qualified Data.Map        as Map
import Data.Set (Set)

import Lvca.Util

import Debug.Trace

data Term
  = Term !Text !(Set Text) ![Term]
  | Var !Text
  deriving (Eq, Show)

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
  -- , _arities :: Map Text Int
  }

makeLenses ''Env

type Check a = ReaderT Env Maybe a

runCheck :: Env -> Check a -> Maybe a
runCheck = flip runReaderT

data Typing = Term :< Term

checkEq :: Term -> Term -> Maybe ()
checkEq (Term t1 _ ts1) (Term t2 _ ts2)
  = if t1 == t2
    then join $ sequence_ <$> pairWith checkEq ts1 ts2
    else Nothing
checkEq (Var a) (Var b) = if a == b then Just () else Nothing
checkEq Var{} Term{} = Just ()
checkEq Term{} Var{} = Just ()

-- | Match a pattern (on the left) with a term (containing no variables).
matchPatternVars :: Term -> Term -> Maybe (Map Text Term)
matchPatternVars (Var v) tm
  = Just $ Map.singleton v tm
matchPatternVars (Term head1 _ args1) (Term head2 _ args2)
  | head1 == head2
  && length args1 == length args2
  = Map.unions <$> traverse (uncurry matchPatternVars) (zip args1 args2)
matchPatternVars _ _
  = Nothing

check :: Typing -> Check ()
check (tm :< ty) = join $ ReaderT $ \Env{_rules} -> getFirst $
  foldMap
    (First . \case
      Rule _ InferenceRule{} -> Nothing
      Rule hyps (CheckingRule (ruleTm :<= ruleTy)) -> do
        tmAssignments <- matchPatternVars ruleTm tm
        tyAssignments <- matchPatternVars ruleTy ty

        traceM $ "hyps: " ++ show hyps
        traceM $ "ruleTm: " ++ show ruleTm
        traceM $ "ruleTy: " ++ show ruleTy
        traceM $ "tmAssignments: " ++ show tmAssignments
        traceM $ "tyAssignments: " ++ show tyAssignments

        Just $ for_ hyps $ \case
          (ctx, CheckingRule  (hypTm :<= hypTy)) -> do
            tm' <- lift $ instantiate tmAssignments hypTm
            ty' <- lift $ instantiate tyAssignments hypTy
            traceM $ "tm': " ++ show tm'
            traceM $ "ty': " ++ show ty'
            local (varTypes <>~ ctx) $
              check $ tm' :< ty'
          (ctx, InferenceRule (hypTm :=> hypTy)) -> do
            tm' <- lift $ instantiate tmAssignments hypTm
            ty' <- local (varTypes <>~ ctx) $ infer tm'
            -- TODO: propagate unification information
            lift $ checkEq hypTy ty')
    _rules

instantiate :: Map Text Term -> Term -> Maybe Term
instantiate env (Term tag names subtms)
  = let env' = Map.withoutKeys env names
    in Term tag names <$> traverse (instantiate env') subtms
instantiate env (Var v)
  = env ^? ix v

infer :: Term -> Check Term
infer tm = join $ ReaderT $ \Env{_rules} -> getFirst $
  foldMap
    (First . \case
      Rule _ CheckingRule{} -> Nothing
      Rule hyps (InferenceRule (ruleTm :=> ruleTy)) -> do
        tmAssignments <- matchPatternVars ruleTm tm

        traceM $ "hyps: " ++ show hyps
        traceM $ "ruleTm: " ++ show ruleTm
        traceM $ "ruleTy: " ++ show ruleTy
        traceM $ "tmAssignments: " ++ show tmAssignments

        Just $ do
          for_ hyps $ \case
            (ctx, CheckingRule  (hypTm :<= hypTy)) -> do
              traceM $ "hypTm: " ++ show hypTm
              traceM $ "hypTy: " ++ show hypTy
              tm' <- lift $ instantiate tmAssignments hypTm
              ty' <- lift $ instantiate tmAssignments hypTy
              traceM $ "tm': " ++ show tm'
              traceM $ "ty': " ++ show ty'
              local (varTypes <>~ ctx) $ check $ tm' :< ty'
            (ctx, InferenceRule (hypTm :=> hypTy)) -> do
              tm' <- lift $ instantiate tmAssignments hypTm
              ty' <- local (varTypes <>~ ctx) $ infer tm'
              -- TODO: propagate unification information
              lift $ checkEq hypTy ty'
          lift $ instantiate tmAssignments ruleTy)
    _rules
