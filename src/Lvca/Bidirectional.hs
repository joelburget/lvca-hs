{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TemplateHaskell        #-}
module Lvca.Bidirectional where

import Data.Foldable             (for_)
import Control.Monad             (join)
import Data.Monoid               (First(First, getFirst))
import Data.Text                 (Text)
import Control.Lens              (makeLenses, (^?), ix)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Class (lift)
import Data.Map                  (Map)
import qualified Data.Map        as Map

import Lvca.Util

import Debug.Trace

data Term
  = Term !Text ![Term]
  | Var !Text
  deriving (Eq, Show)

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
  { _hypotheses :: ![TypingClause]
  , _conclusion :: !TypingClause
  }

newtype Env = Env { _rules :: [Rule] }
newtype VarTys = VarTys { _varTypes :: Map Text Term }

makeLenses ''Env
makeLenses ''VarTys

type Check a = ReaderT Env (StateT VarTys Maybe) a

-- XXX what to do with VarTys?
runCheck :: Env -> Check a -> Maybe a
runCheck env = (`evalStateT` VarTys Map.empty) . (`runReaderT` env)

data Typing = Term :< Term

unify :: Term -> Term -> Maybe Term
unify (Term t1 ts1) (Term t2 ts2)
  = if t1 == t2
    then Term t1 <$> (join $ fmap sequence $ pairWith unify ts1 ts2)
    else Nothing
-- XXX this is wrong
unify (Var a) (Var b) = if a == b then Just (Var a) else Nothing
unify Var{} t@Term{} = Just t
unify t@Term{} Var{} = Just t

-- saturate :: Term -> Check Term
-- saturate (Term tag subtms) = Term tag <$> traverse saturate subtms
-- saturate v@(Var name) = do
--   ty <- use $ varTypes . at name
--   case ty of
--     Nothing  -> pure v
--     Just val -> pure val

-- | Match a pattern (on the left) with a term (containing no variables).
matchPatternVars :: Term -> Term -> Maybe (Map Text Term)
matchPatternVars (Var v) tm
  = Just $ Map.singleton v tm
matchPatternVars (Term head1 args1) (Term head2 args2)
  | head1 == head2
  && length args1 == length args2
  = Map.unions <$> traverse (uncurry matchPatternVars) (zip args1 args2)
matchPatternVars _ _
  = Nothing

liftMaybe :: Maybe a -> Check a
liftMaybe = lift . lift

check :: Typing -> Check ()
check (tm :< ty) = join $ ReaderT $ \Env{_rules} -> lift $ getFirst $
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
          CheckingRule  (hypTm :<= hypTy) -> do
            tm' <- liftMaybe $ instantiate tmAssignments hypTm
            ty' <- liftMaybe $ instantiate tyAssignments hypTy
            traceM $ "tm': " ++ show tm'
            traceM $ "ty': " ++ show ty'
            check $ tm' :< ty'
          InferenceRule (hypTm :=> hypTy) -> do
            tm' <- liftMaybe $ instantiate tmAssignments hypTm
            ty' <- infer tm'
            -- TODO: propagate unification information
            void $ liftMaybe $ unify hypTy ty')
    _rules

instantiate :: Map Text Term -> Term -> Maybe Term
instantiate env (Term tag subtms) = Term tag <$> traverse (instantiate env) subtms
instantiate env (Var v)          = env ^? ix v

infer :: Term -> Check Term
infer tm = join $ ReaderT $ \Env{_rules} -> lift $ getFirst $
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
            CheckingRule  (hypTm :<= hypTy) -> do
              traceM $ "hypTm: " ++ show hypTm
              traceM $ "hypTy: " ++ show hypTy
              tm' <- liftMaybe $ instantiate tmAssignments hypTm
              ty' <- liftMaybe $ instantiate tmAssignments hypTy
              traceM $ "tm': " ++ show tm'
              traceM $ "ty': " ++ show ty'
              check $ tm' :< ty'
            InferenceRule (hypTm :=> hypTy) -> do
              tm' <- liftMaybe $ instantiate tmAssignments hypTm
              ty' <- infer tm'
              -- TODO: propagate unification information
              void $ liftMaybe $ unify hypTy ty'
          liftMaybe $ instantiate tmAssignments ruleTy)
          -- saturate ruleTy)
    _rules
