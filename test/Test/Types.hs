module Test.Types
  ( toPatternTests
  , genTerm
  , prop_serialise_identity
  ) where

import           Codec.Serialise
import           Control.Lens          (ix, (<&>), (^?!))
import           Data.Foldable         (foldrM)
import           Data.Functor.Identity
import qualified Data.Map              as Map
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Void             (Void)
import           EasyTest
import           Hedgehog
  (GenT, MonadGen) -- , Property, forAll, property, (===))
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range

import           Lvca.Types            hiding (valences)

toPatternTests :: Test
toPatternTests = scope "toPattern" $
  let toPat :: Operator -> Pattern Void
      toPat = toPattern
  in tests
    [ example $
      toPat (Operator "num" (Arity []) "numbers")
      ===
      PatternTm "num" []
    , example $
      toPat (Operator "plus"  (Arity ["Exp", "Exp"]) "addition")
      ===
      PatternTm "plus" [PatternAny, PatternAny]
    , example $
      toPat (Operator "num" (ExternalArity "num") "numbers")
      ===
      PatternTm "num" [ PatternPrimVal Nothing ]
    ]

genName :: MonadGen m => m Text
genName = Text.cons
  <$> Gen.alpha
  <*> Gen.text (Range.exponential 0 500) Gen.alphaNum

genTerm
  :: forall m a.
     MonadGen m
  => SyntaxChart
  -> Sort
  -> (SortName -> Maybe (m a))
  -> m (Term a)
genTerm _chart (External name) genPrim = case genPrim name of
  Nothing  -> Gen.discard
  Just gen -> PrimValue <$> gen
genTerm chart@(SyntaxChart chart') (SortAp sortHead sortArgs) genPrim = do
    let SortDef vars operators = chart' ^?! ix sortHead
        sortVarVals = Map.fromList $ zip vars sortArgs

        opNames = _operatorName <$> operators

        nonrec =
          [ do
               name <- genName
               if name `elem` opNames
                 then Gen.discard
                 else pure $ Var name
          ]

        genArity :: [Valence] -> m [Term a]
        genArity = foldrM
          -- TODO: handle applied sorts
          (\valence valences' -> case valence of
            Valence binders sort -> do
              tm <- genTerm chart (sortSubst sortVarVals sort) genPrim
              case binders of
                [] -> pure $ tm : valences'
                _  -> do
                  names <- Gen.list (Range.singleton (length binders)) genName
                  pure $ Binding names tm : valences'
          )
          []

        genTerm' :: Text -> [Valence] -> m (Term a)
        genTerm' name valences = Term name <$> genArity valences

        rec = operators <&> \(Operator name (Arity valences) _desc) ->
          genTerm' name valences

    Gen.sized $ \n ->
      if n <= 1 then
        Gen.choice nonrec
      else
        Gen.choice $ nonrec ++ fmap Gen.small rec

prop_serialise_identity
  :: (Show a, Eq a, Serialise a)
  => SyntaxChart
  -> Sort
  -> (SortName -> Maybe (GenT Identity a))
  -> Test
prop_serialise_identity chart sort aGen = property $ do
  tm <- forAll $ genTerm chart sort aGen
  -- (this is serialiseIdentity from Codec.Serialise.Properties)
  tm === (deserialise . serialise) tm

-- TODO:
-- patP_round_trip_prop :: Eq1 f => Gen (Fix (PatVarF :+: f)) -> Property
-- patP_round_trip_prop gen = property $ do
--   x <- forAll genPat
--   preview patP (review patP x) === Just x

-- valP_round_trip_prop :: Eq1 f => Gen (Fix f) -> Property
-- valP_round_trip_prop = property $ do
--   x <- forAll genVal
--   preview valP (review valP x) === Just x
