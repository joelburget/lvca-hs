module Test.ParseTerm where

import           Control.Monad.Reader
import           Data.Text                             (Text, unpack)
import           Data.Text.Prettyprint.Doc
  (Pretty(pretty), defaultLayoutOptions, layoutPretty)
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           Data.Void                             (Void)
import           EasyTest                              (Test, expectEq)
import           Hedgehog                              hiding
  (Test, Var, concrete)
import           Text.Earley                           (fullParses)
import           Text.Megaparsec
  (errorBundlePretty, parseMaybe, runParser)

import           Lvca.EarleyParseTerm
import           Lvca.ParseTerm
import           Lvca.Types
import           Test.Types

prop_parse_abstract_pretty
  :: (Show a, Pretty a, Eq a)
  => SyntaxChart
  -> Sort
  -> (SortName -> Maybe (Gen a))
  -> ExternalParsers a
  -> Property
prop_parse_abstract_pretty chart sort aGen aParsers = property $ do
  tm <- forAll $ genTerm chart sort aGen
    -- (Just (Gen.int Range.exponentialBounded))

  let pretty' = renderStrict . layoutPretty defaultLayoutOptions . pretty
      parse'  = parseMaybe $ runReaderT standardParser $
        ParseEnv chart sort TaggedExternals aParsers

  annotate $ unpack $ pretty' tm
  parse' (pretty' tm) === Just tm

prop_parse_concrete_pretty :: SyntaxChart -> Sort -> ConcreteSyntax -> Property
prop_parse_concrete_pretty chart sort concrete = property $ do
  tm <- forAll $ genTerm chart sort (const Nothing)

  let pretty' tm' = renderStrict $ layoutPretty defaultLayoutOptions $
        runReader (prettyTm tm') (-1, concrete)
      parse' str = case fullParses (concreteParser concrete) str of
        ([parsed], _) -> Just parsed
        _             -> Nothing

  annotate $ unpack $ pretty' tm
  parse' (pretty' tm) === Just tm

standardParseTermTest
  :: (Eq a, Show a)
  => ParseEnv a -> Text -> Term a -> Test ()
standardParseTermTest env str tm =
  case runParser (runReaderT standardParser env) "(test)" str of
    Left err       -> fail $ errorBundlePretty err
    Right parsedTm -> expectEq parsedTm tm

earleyConcreteParseTermTest
  :: ConcreteSyntax -> Text -> Term Void -> Test ()
earleyConcreteParseTermTest concrete str tm =
  case fullParses (concreteParser concrete) str of
    ([parsed], _report) -> expectEq parsed tm
    (_,        report') -> fail $ show report'
