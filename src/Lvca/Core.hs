module Lvca.Core where

import Control.Lens hiding ((??))
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (First(First, getFirst))
import Data.String (IsString(fromString))
import Data.Text (Text)
import           Data.Text.Prettyprint.Doc

import Lvca.Types (pattern (:->))
import Lvca.Util (pair, pairWith, (??), (???))

-- TODO: consistency btw Var / Text
newtype Var = Var Text
  deriving (Eq, Ord, Show, IsString)

data Literal
  = LitText !Text
  | LitInteger !Integer
  deriving (Eq, Show)

instance Num Literal where
  fromInteger = LitInteger
  (+)         = error "TODO"
  (-)         = error "TODO"
  (*)         = error "TODO"
  abs         = error "TODO"
  signum      = error "TODO"

instance IsString Literal where
  fromString = LitText . fromString

-- Q: how do lambdas fit in here?
data Val
  = ValTm !Text ![Val]
  | ValLit !Literal
  | ValPrimop !Text
  | ValLam ![Var] !Core
  deriving (Eq, Show)

instance Num Val where
  fromInteger = ValLit . fromInteger
  (+)         = error "TODO"
  (-)         = error "TODO"
  (*)         = error "TODO"
  abs         = error "TODO"
  signum      = error "TODO"

instance IsString Val where
  fromString = ValLit . fromString

data Core
  = CoreVar !Var
  | CoreVal !Val
  | App !Core ![Core]
  | Lam ![Text] !Core
  | Case !Core !Ty ![ (Pat, Core) ]
  -- everything from ghc core except
  -- * let
  -- * cast
  -- * tick
  -- * type
  -- * coercion

  -- Temporary?
  | Metavar !Text
  deriving (Eq, Show)

instance Pretty Core where
  pretty _ = "TODO: Pretty Core"

instance Num Core where
  fromInteger = CoreVal . ValLit . fromInteger
  (+)         = error "TODO"
  (-)         = error "TODO"
  (*)         = error "TODO"
  abs         = error "TODO"
  signum      = error "TODO"

-- TODO: Patterns matching lambdas?
data Pat
  = PatternTm !Text [Pat]
  | PatternVar !(Maybe Text)
  | PatternLit !Literal
  | PatternDefault -- TODO is this redundant with PatternVar? Switch to AltCon?
  deriving (Eq, Show)

instance Num Pat where
  fromInteger = PatternLit . fromInteger
  (+)         = error "TODO"
  (-)         = error "TODO"
  (*)         = error "TODO"
  abs         = error "TODO"
  signum      = error "TODO"

-- TODO: pattern coverage checker (type-informed)

data Ty = Ty -- TODO
  deriving (Eq, Show)

matchBranch :: Val -> Pat -> Core -> Maybe (Map Var Val, Core)
matchBranch val pat core = (,core) <$> matchBranch' val pat

matchBranch' :: Val -> Pat -> Maybe (Map Var Val)
matchBranch' (ValTm tag1 vals) (PatternTm tag2 pats)
  | tag1 == tag2
  = do mMaps <- pairWith matchBranch' vals pats
       Map.unions <$> sequence mMaps
  | otherwise
  = Nothing
matchBranch' (ValLit l1) (PatternLit l2)
  | l1 == l2
  = Just Map.empty
  | otherwise
  = Nothing
matchBranch' val (PatternVar v)
  | Just v' <- v
  = Just $ Map.singleton (Var v') val
  | otherwise
  = Just Map.empty
matchBranch' _val PatternDefault
  = Just Map.empty
-- TODO: match ValLam?
matchBranch' _ _
  = Nothing

-- Q:
-- * this holds vals, also have term context?
-- * we probably want to also have some set of effectful builtins
type Eval = ReaderT (Map Var Val) (Either String)

evalCore' :: Core -> Eval Val
evalCore' = \case
  CoreVar v -> view (at v) ??? "couldn't find var " ++ show v
  CoreVal v -> pure v
  App f args -> do
    f'    <- evalCore' f
    args' <- traverse evalCore' args -- Q: CBV?
    case f' of
      ValLam bindNames body -> do
        bindings <- pair bindNames args'
          ?? "wrong number of arguments to a lambda"
        local (Map.union (Map.fromList bindings)) (evalCore' body)
      ValPrimop name
        | Just handler <- Map.lookup name primops
        -> handler args'
      bad -> lift $ Left $ "unexpected non-function: " ++ show bad
  Lam binders body -> pure $ ValLam (Var <$> binders) body
  Case tm _ty branches -> do
    val <- evalCore' tm
    (newVars, branch) <- getFirst (
      foldMap (fmap First $ uncurry $ matchBranch val) branches
      ) ?? "couldn't find a matching branch"
    local (Map.union newVars) (evalCore' branch)

primops :: Map Text ([Val] -> Eval Val)
primops = Map.fromList
  [ "add" :-> \case
    [ValLit (LitInteger a), ValLit (LitInteger b)]
      -> pure $ ValLit (LitInteger (a + b))
    _ -> lift $ Left "args to add"
  , "sub" :-> \case
    [ValLit (LitInteger a), ValLit (LitInteger b)]
      -> pure $ ValLit (LitInteger (a - b))
    _ -> lift $ Left "args to sub"
  ]

runEval :: Eval a -> Either String a
runEval a = runReaderT a Map.empty

evalCore :: Core -> Either String Val
evalCore = runEval . evalCore'


