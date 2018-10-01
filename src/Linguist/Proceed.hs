{-# LANGUAGE ViewPatterns      #-}
module Linguist.Proceed
  ( eval
  ) where

import Data.Functor.Compose
import           Control.Lens         hiding (from, to, (??))
import           Control.Monad.Except (throwError)
import           Control.Monad.Reader
import           Data.Foldable        (toList)
import qualified Data.List            as List
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromMaybe)
import           Data.Sequence             (Seq)
import qualified Data.Sequence        as Seq
import           Data.Text            (Text)
import qualified Data.Text            as Text
import Data.Functor.Foldable (Fix(Fix), unfix)
import Control.Monad.Free

import           Linguist.Types
import           Linguist.Languages.MachineModel
import           Linguist.Util        ((??))
import Linguist.FunctorUtil

import Debug.Trace

type EvalEnv a b =
  ( SyntaxChart
  , DenotationChart a b
  , ReifiedPrism' b Text
  , Text -> Maybe (Seq (Term a) -> Term a)
  , Map Text (Term a)
  )

eval
  :: (Eq a, Show a, Show b)
  => SortName
  -> SyntaxChart
  -> DenotationChart a b
  -> Prism' b Text
  -> (Text -> Maybe (Seq (Term a) -> Term a))
  -> Term a
  -> Either String (Term a)
eval sort sChart dChart p evalPrim tm
  = runReaderT (eval' sort tm) (sChart, dChart, Prism p, evalPrim, Map.empty)

-- findMatch'
--   :: (Eq a, Show a, Show b)
--   => DenotationChart a b
--   -> Term a
--   -> Prism' b Text
--   -> Prism' (Term b) (f (Free (MeaningF f) Text))
--   -> Matching a (Subst a, Free (MeaningF f) Text)

findMatch'
  :: (Eq a, Show a, Show b)
  => DenotationChart a b
  -> Term a
  -> Prism' b Text
  -> Matching a (Subst a, Free (MeaningF :+: f) Text)
findMatch' chart tm textP = do
  (subst, meaningTm) <- findMatch chart tm
  traceM "found match in findMatch'"
  traceM $ "meaning: " ++ show meaningTm
  meaning            <- lift $ meaningTm ^? meaningTermP textP (error "TODO")
  traceM "found meaning in findMatch'"
  pure (subst, meaning)

-- TODO: we're confused about sorts. here we basically ignore them.
-- PatternPrimVar includes a sort. All sorts are mixed up in dynamics.
-- findMatch should maybe return the sort it finds.
eval'
  :: (Eq a, Show a, Show b)
  => SortName -> Term a -> ReaderT (EvalEnv a b) (Either String) (Term a)
eval' sort = \case
  tm@(Term _name subtms) -> do
    (sChart, dChart, Prism p, evalMeaningPrimitive, vars) <- ask
    let matchesEnv = MatchesEnv sChart sort vars
    case runReaderT (findMatch' dChart tm p) matchesEnv of
      Just ( subst@(Subst assignment correspondences)
           , Free (InL (Bind bindFrom' patName' bindTo'))
           ) -> do

        (error "TODO")

      Nothing -> throwError $ "cound't find match for term " ++ show tm

  Binding{} -> throwError "can't evaluate exposed binding"
  Var name -> do
    tm <- view (_5 . at name)
    case tm of
      Just tm' -> pure tm'
      Nothing  -> throwError $ "unable to look up var " ++ Text.unpack name
  tm@PrimValue{} -> pure tm

-- Only to be used in evaluation -- not proceed.
substIn :: Map Text (Term a) -> Term a -> Term a
substIn vals tm = case tm of
  Term name subtms -> Term name $ substIn vals <$> subtms
  Binding boundNames body -> case filter (`Map.notMember` vals) boundNames of
    []          -> body
    boundNames' -> Binding boundNames' $ substIn vals body
  Var name
    | Just val <- vals ^? ix name
    -> val
    | otherwise          -> tm
  PrimValue{} -> tm
