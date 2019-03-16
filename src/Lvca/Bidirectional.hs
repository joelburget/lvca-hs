{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TemplateHaskell        #-}
module Lvca.Bidirectional where

import Control.Applicative       ((<|>))
import Control.Monad             (join)
import Control.Lens              (makeLenses, (^?), at, ix, (<>~), view)
import Control.Monad.Reader
import Control.Monad.Trans.Class (lift)
import Data.Foldable             (for_)
import Data.Map                  (Map)
import qualified Data.Map        as Map
import Data.Monoid               (First(First, getFirst))
import Data.Set (Set)
import Data.Text                 (Text)

import Lvca.Util

-- import Debug.Trace

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

        -- traceM "check\n====="
        -- traceM $ "hyps: " ++ show hyps
        -- traceM $ "tm: " ++ show tm
        -- traceM $ "ruleTm: " ++ show ruleTm
        -- traceM $ "ty: " ++ show ty
        -- traceM $ "ruleTy: " ++ show ruleTy
        -- traceM $ "tmAssignments: " ++ show tmAssignments
        -- traceM $ "tyAssignments: " ++ show tyAssignments

        Just $ for_ hyps $ \case
          (ctx, CheckingRule  (hypTm :<= hypTy)) -> do
            tm'  <- lift $ instantiate tmAssignments hypTm
            ty'  <- lift $ instantiate tyAssignments hypTy
            ctx' <- lift $ traverse (instantiate tyAssignments) ctx
            local (varTypes <>~ ctx') $ check $ tm' :< ty'
          (ctx, InferenceRule (hypTm :=> hypTy)) -> do
            tm'  <- lift $ instantiate tmAssignments hypTm
            ty'  <- lift $ instantiate tyAssignments hypTy
            ctx' <- lift $ traverse (instantiate tyAssignments) ctx
            ty'' <- local (varTypes <>~ ctx') $ infer tm'
            lift $ checkEq ty' ty'')
    _rules

instantiate :: Map Text Term -> Term -> Maybe Term
instantiate env (Term tag names subtms)
  = Term tag names <$> traverse (instantiate env') subtms
      where env' = Map.withoutKeys env names
instantiate env (Var v)
  = env ^? ix v

infer :: Term -> Check Term
infer tm = (join $ ReaderT $ \Env{_rules} -> getFirst $
  foldMap
    (First . \case
      Rule _ CheckingRule{} -> Nothing
      Rule hyps (InferenceRule (ruleTm :=> ruleTy)) -> do
        tmAssignments <- matchPatternVars ruleTm tm

--         traceM "infer\n====="
--         traceM $ "hyps: " ++ show hyps
--         traceM $ "ruleTm: " ++ show ruleTm
--         traceM $ "ruleTy: " ++ show ruleTy
--         traceM $ "tmAssignments: " ++ show tmAssignments

        Just $ do
          for_ hyps $ \case
            (ctx, CheckingRule  (hypTm :<= hypTy)) -> do
              tm' <- lift $ instantiate tmAssignments hypTm
              ty' <- lift $ instantiate tmAssignments hypTy
              local (varTypes <>~ ctx) $ check $ tm' :< ty'
            (ctx, InferenceRule (hypTm :=> hypTy)) -> do
              tm' <- lift $ instantiate tmAssignments hypTm
              ty' <- local (varTypes <>~ ctx) $ infer tm'
              lift $ checkEq hypTy ty'
          lift $ instantiate tmAssignments ruleTy)
    _rules) <|> ctxInfer tm

ctxInfer :: Term -> Check Term
ctxInfer = lift <=< \case
  Term{} -> pure Nothing
  Var v  -> view $ varTypes . at v
