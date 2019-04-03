module Lvca.DenotationChart where

import Control.Lens
import Control.Monad.Reader
import Data.Monoid (First(First, getFirst))
import           Data.Text.Prettyprint.Doc
import qualified Data.Map as Map

import Lvca.Core  (Core(..), Var(..))
import Lvca.Types
  (Matching, Pattern, PatternCheckResult(..), IsRedudant(..), Term, MatchResult(..), Scope(..), completePattern, minus, matches, runMatches)
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

-- TODO: use Either
termToCore :: (Eq a, Show a) => DenotationChart a -> Term a -> Maybe Core
termToCore chart tm = do
  (matchRes, protoCore) <- runMatches undefined undefined (findMatch chart tm)
  fillIn matchRes protoCore

fillIn :: MatchResult a -> Core -> Maybe Core
fillIn (MatchResult assignments) = \case
  CoreVar (Var name) -> case Map.lookup name assignments of
    Just (Scope [] tm) -> undefined
    _                  -> Nothing
