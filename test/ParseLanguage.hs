module Test.ParseLanguage

import           Data.Text.Prettyprint.Doc             (defaultLayoutOptions,
                                                        layoutPretty, Pretty(pretty))
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           EasyTest             (Test, expectEq)
import           Hedgehog             hiding (Test, Var)

prop_parse_pretty
  :: (Show a, Pretty a, Eq a)
  => SyntaxChart
  -> Sort
  -> (SortName -> Maybe (Gen a))
  -> ExternalParsers a
  -> Property
prop_parse_pretty chart sort aGen aParsers = property $ do
  tm <- forAll $ genTerm chart sort aGen
    -- (Just (Gen.int Range.exponentialBounded))

  let pretty' = renderStrict . layoutPretty defaultLayoutOptions . pretty
      parse'  = parseMaybe $ runReaderT standardParser $
        ParseEnv chart sort TaggedExternals aParsers

  annotate $ unpack $ pretty' tm
  parse' (pretty' tm) === Just tm

parseTest
  :: (Eq a, Show a)
  => ParseEnv a -> Text -> Term a -> Test ()
parseTest env str tm =
  case runParser (runReaderT standardParser env) "(test)" str of
    Left err       -> fail $ errorBundlePretty err
    Right parsedTm -> expectEq parsedTm tm
