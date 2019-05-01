module Lvca.DenotationChart where

import Data.Text (Text)
import Control.Lens hiding ((??))
import Control.Monad.Reader
import Data.Monoid (First(First, getFirst))
import           Data.Text.Prettyprint.Doc
import           Data.Traversable
import           Data.Map (Map)
import qualified Data.Map as Map

import Lvca.Core  (Core(..), Val(..))
import qualified Lvca.Core as Core
import Lvca.Types (Term(..), Scope(..))
import Lvca.Util

-- | Denotation charts
--
-- A denotation chart maps from patterns to their denotation. Patterns are
-- checked from top to bottom.
--
-- We check for completeness and redundancy using a very similar algorithm to
-- Haskell's pattern match checks. These could also be compiled efficiently in
-- a similar way.
newtype DenotationChart = DenotationChart [(DenotationPat, Core)]
  deriving Show

data DenotationPat
  = DPatternTm !Text ![DenotationScopePat]
  | DVar !(Maybe Text)
  deriving Show

data DenotationScopePat
  = DenotationScopePat ![Text] !DenotationPat
  deriving Show

instance Pretty DenotationPat where
  pretty = \case
    DPatternTm name subpats
      -> pretty name <> parens (hsep $ punctuate semi $ fmap pretty subpats)
    DVar Nothing
      -> "_"
    DVar (Just v)
      -> pretty v

instance Pretty DenotationScopePat where
  pretty (DenotationScopePat binders pat)
    = hsep $ punctuate dot $ fmap pretty binders <> [ pretty pat ]

instance Pretty DenotationChart where
  pretty (DenotationChart rows) = vsep $ rows <&> \(pat, tm) ->
    "[[ " <> pretty pat <> " ]] = " <> pretty tm

newtype TmAssocs = TmAssocs [ScopeAssocs]
  deriving (Semigroup, Monoid)

data ScopeAssocs
  = ScopeAssocs ![BinderAssoc] !TmAssocs

data BinderAssoc = BinderAssoc
  { _patName :: !Text
  , _tmName  :: !Text
  }

findMatch
  :: DenotationChart
  -> Term a
  -> Maybe (TmAssocs, Map Text (Term a), Core)
findMatch (DenotationChart pats) tm
  = getFirst $ foldMap (First . uncurry (matches tm)) pats

matches
  :: Term a
  -> DenotationPat
  -> Core
  -> Maybe (TmAssocs, Map Text (Term a), Core)
matches tm pat core = matchTm tm pat <&> \(a, b) -> (a, b, core)

matchTm
  :: Term a -> DenotationPat -> Maybe (TmAssocs, Map Text (Term a))
matchTm (Term tag2 subtms) (DPatternTm tag1 subpats)
  | tag1 == tag2
  = do submatches <- pairWith matchScope subtms subpats >>= sequence
       let (scopeAssocs, submatches') = unzip submatches
       pure (TmAssocs scopeAssocs, mconcat submatches')
       -- mconcat <$> sequence submatches
  | otherwise
  = Nothing
matchTm _  DPatternTm{} -- TODO: is this right when term is a var?
  = Nothing
matchTm _ (DVar Nothing)
  = Just (mempty, Map.empty)
matchTm tm (DVar (Just v))
  = Just (mempty, Map.singleton v tm)

matchScope
  :: Scope a
  -> DenotationScopePat
  -> Maybe (ScopeAssocs, Map Text (Term a))
matchScope (Scope binders tm) (DenotationScopePat patBinders pat)
  | Just binderPairs <- pair patBinders binders
  -- TODO: we need a richer notion of binder pairing!
  = do (assocs, tmMatches) <- matchTm tm pat
       pure
         ( ScopeAssocs (fmap (uncurry BinderAssoc) binderPairs) assocs
         , tmMatches
         )
  | otherwise
  = Nothing

type Translator = ReaderT DenotationChart (Either String)

termToCore :: Pretty a => Term a -> Translator Core
termToCore tm = do
  dynamics                      <- ask
  case tm of
    Var v -> pure $ CoreVar $ Core.Var v
    _ -> do
      (assocs, matchRes, protoCore) <- findMatch dynamics tm
        ?? "failed to find match for " ++ show (pretty tm)
      fillInCore (assocs, matchRes) protoCore

fillInCore
  :: Pretty a => (TmAssocs, Map Text (Term a)) -> Core -> Translator Core
fillInCore mr@(assocs, assignments) c = case c of
  Metavar name -> case Map.lookup name assignments of
    Just tm -> termToCore tm
    Nothing -> lift $ Left "TODO"
  CoreVar{} -> pure c -- TODO: change name?
  CoreVal val -> CoreVal <$> fillInVal mr val
  App fun args -> App <$> fillInCore mr fun <*> traverse (fillInCore mr) args
  Lam binders core -> Lam binders <$> fillInCore mr core
  Case scrutinee ty branches -> do
    scrutinee' <- fillInCore mr scrutinee
    branches'  <- for branches $ \(pat, core) -> (pat,) <$> fillInCore mr core
    pure $ Case scrutinee' ty branches'

fillInVal
  :: Pretty a => (TmAssocs, Map Text (Term a)) -> Val -> Translator Val
fillInVal mr val = case val of
  ValTm tag vals      -> ValTm tag <$> traverse (fillInVal mr) vals
  ValLit{}            -> pure val
  ValPrimop{}         -> pure val
  ValLam binders core -> ValLam binders <$> fillInCore mr core
