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

findMatch
  :: DenotationChart
  -> Term
  -> Maybe ([(Text, Text)], Map Text Term, Core)
findMatch (DenotationChart pats) tm
  = getFirst $ foldMap (First . uncurry (matches tm)) pats

matches
  :: Term
  -> DenotationPat
  -> Core
  -> Maybe ([(Text, Text)], Map Text Term, Core)
matches (Var v) _ _ = Just ([], Map.empty, CoreVar (Core.Var v))
matches tm pat core = matches' tm pat <&> \(a, b) -> (a, b, core)

matches'
  :: Term -> DenotationPat -> Maybe ([(Text, Text)], Map Text Term)
matches' (Term tag2 subtms) (DPatternTm tag1 subpats)
  | tag1 == tag2
  = do submatches <- pairWith matches'' subtms subpats
       mconcat <$> sequence submatches
  | otherwise
  = Nothing
matches' _  DPatternTm{} -- TODO: is this right when term is a var?
  = Nothing
matches' _ (DVar Nothing)
  = Just ([], Map.empty)
matches' tm (DVar (Just v))
  = Just ([], Map.singleton v tm)

matches''
  :: Scope
  -> DenotationScopePat
  -> Maybe ([(Text, Text)], Map Text Term)
matches'' (Scope binders tm) (DenotationScopePat patBinders pat)
  | Just binderPairs <- pair patBinders binders
  -- TODO: we need a richer notion of binder pairing!
  = do (assocs, tmMatches) <- matches' tm pat
       pure (binderPairs <> assocs, tmMatches)
  | otherwise
  = Nothing

type Translator = ReaderT DenotationChart (Either String)

termToCore :: Term -> Translator Core
termToCore tm = do
  dynamics                      <- ask
  (assocs, matchRes, protoCore) <- findMatch dynamics tm
    ?? "failed to find match for " ++ show (pretty tm)
  fillInCore (assocs, matchRes) protoCore

fillInCore :: ([(Text, Text)], Map Text Term) -> Core -> Translator Core
fillInCore mr@(assocs, assignments) c = case c of
  Metavar name -> case Map.lookup name assignments of
    Just tm -> termToCore tm
    Nothing -> lift $ Left "TODO"
  CoreVar{} -> pure c
  CoreVal val -> CoreVal <$> fillInVal mr val
  App fun args -> App <$> fillInCore mr fun <*> traverse (fillInCore mr) args
  Lam binders core -> Lam binders <$> fillInCore mr core
  Case scrutinee ty branches -> do
    scrutinee' <- fillInCore mr scrutinee
    branches'  <- for branches $ \(pat, core) -> (pat,) <$> fillInCore mr core
    pure $ Case scrutinee' ty branches'

fillInVal :: ([(Text, Text)], Map Text Term) -> Val -> Translator Val
fillInVal mr val = case val of
  ValTm tag vals      -> ValTm tag <$> traverse (fillInVal mr) vals
  ValLit{}            -> pure val
  ValPrimop{}         -> pure val
  ValLam binders core -> ValLam binders <$> fillInCore mr core
