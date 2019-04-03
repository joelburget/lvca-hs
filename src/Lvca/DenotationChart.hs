module Lvca.DenotationChart where

import Control.Lens
import Control.Monad.Reader
import Data.Monoid (First(First, getFirst))
import           Data.Text.Prettyprint.Doc
import           Data.Traversable
import qualified Data.Map as Map

import Lvca.Core  (Core(..), Val(..))
import Lvca.Types
  (SortName, SyntaxChart, Matching, Pattern, PatternCheckResult(..), IsRedudant(..), Term, MatchResult(..), Scope(..), completePattern, minus, matches, runMatches)
import Lvca.Util

-- | Denotation charts
--
-- A denotation chart maps from patterns to their denotation. Patterns are
-- checked from top to bottom.
--
-- We check for completeness and redundancy using a very similar algorithm to
-- Haskell's pattern match checks. These could also be compiled efficiently in
-- a similar way.
newtype DenotationChart a = DenotationChart [(Pattern a, Core)]
  deriving Show

instance Pretty a => Pretty (DenotationChart a) where
  pretty (DenotationChart rows) = vsep $ rows <&> \(pat, tm) ->
    "[[ " <> pretty pat <> " ]] = " <> pretty tm

-- | Check a chart for uncovered and overlapping patterns.
patternCheck
  :: forall a. Eq a => DenotationChart a -> Matching a (PatternCheckResult a)
patternCheck (DenotationChart chart) = do
  unmatched <- completePattern -- everything unmatched
  (overlaps, unmatched') <- mapAccumM
    (\unmatchedAcc (pat, _denotation) -> go unmatchedAcc pat)
    unmatched
    chart
  pure $ PatternCheckResult unmatched' overlaps

  -- go:
  -- - takes the set of uncovered values
  -- - returns the (set of covered values, set of remaining uncovered values)
  where go :: Pattern a
           -> Pattern a
           -> Matching a ((Pattern a, IsRedudant), Pattern a)
        go unmatched pat = do
          pat' <- unmatched `minus` pat
          let redundant = if pat == pat' then IsRedudant else IsntRedundant
          pure ((pat, redundant), pat')

findMatch
  :: (Eq a, Show a)
  => DenotationChart a
  -> Term a
  -> Matching a (MatchResult a, Core)
findMatch (DenotationChart pats) tm = do
  env <- ask
  let results = pats <&> \(pat, rhs) ->
        runReaderT (matches pat tm) env & _Just %~ (, rhs)

  lift $ getFirst $ foldMap First results

type Translator a = ReaderT (DenotationChart a, SyntaxChart, SortName) Maybe

-- TODO: use Either
termToCore :: (Eq a, Show a) => Term a -> Translator a Core
termToCore tm = do
  (dynamics, syntax, sort) <- ask
  (matchRes, protoCore)
    <- lift $ runMatches syntax sort $ findMatch dynamics tm
  fillInCore matchRes protoCore

fillInCore :: (Eq a, Show a) => MatchResult a -> Core -> Translator a Core
fillInCore mr@(MatchResult assignments) c = case c of
  Metavar name -> case Map.lookup name assignments of
    Just (Scope [] tm) -> termToCore tm
    -- TODO!
    _                  -> lift Nothing
  CoreVar{} -> pure c
  CoreVal val -> CoreVal <$> fillInVal mr val
  App fun args -> App <$> fillInCore mr fun <*> traverse (fillInCore mr) args
  Lam binders core -> Lam binders <$> fillInCore mr core
  Case scrutinee ty branches -> do
    scrutinee' <- fillInCore mr scrutinee
    branches'  <- for branches $ \(pat, core) -> (pat,) <$> fillInCore mr core
    pure $ Case scrutinee' ty branches'

fillInVal :: (Eq a, Show a) => MatchResult a -> Val -> Translator a Val
fillInVal mr val = case val of
  ValTm tag vals -> ValTm tag <$> traverse (fillInVal mr) vals
  ValLit{} -> pure val
  ValPrimop{} -> pure val
  ValLam binders core -> ValLam binders <$> fillInCore mr core
