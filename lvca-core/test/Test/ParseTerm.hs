module Test.ParseTerm where

import           Control.Monad.Reader
import           Data.Text                             (Text, unpack)
import           Data.Text.Prettyprint.Doc
  (Pretty(pretty), defaultLayoutOptions, layoutPretty)
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           Data.Void                             (Void)
import           EasyTest
import           GHC.Stack                             (withFrozenCallStack)
import           Hedgehog                              (Gen)
import           Text.Earley                           (fullParses)
import           Text.Megaparsec
  (errorBundlePretty, parseMaybe, runParser)

import           Lvca.EarleyParseTerm
import           Lvca.ParseTerm
import           Lvca.Printer
import           Lvca.TokenizeConcrete (tokenizeConcrete)
import           Lvca.Types
import           Test.Types

prop_parse_abstract_pretty
  :: (Show a, Pretty a, Eq a)
  => SyntaxChart
  -> Sort
  -> (SortName -> Maybe (Gen a))
  -> ExternalParsers a
  -> Test
prop_parse_abstract_pretty chart sort aGen aParsers = property $ do
  tm <- forAll $ genTerm chart sort aGen
    -- (Just (Gen.int Range.exponentialBounded))

  let pretty' = renderStrict . layoutPretty defaultLayoutOptions . pretty
      parse'  = parseMaybe $ runReaderT standardParser' $
        ParseEnv chart sort TaggedExternals aParsers

  annotate $ unpack $ pretty' tm
  parse' (pretty' tm) === Just tm

prop_parse_concrete_pretty :: SyntaxChart -> Sort -> ConcreteSyntax -> Test
prop_parse_concrete_pretty chart sort concrete = property $ do
  tm <- forAll $ genTerm chart sort (const Nothing)

  let pretty' tm' = renderStrict $ layoutPretty defaultLayoutOptions $
        runReader (prettyTm tm') (-1, concrete)
      parse' str = do
        tmTokens <- case tokenizeConcrete (keywords concrete) str of
          Left err   -> Nothing
          Right good -> Just good
        case fullParses (concreteParser concrete) tmTokens of
          ([parsed], _) -> Just parsed
          _             -> Nothing

  annotate $ unpack $ pretty' tm
  parse' (pretty' tm) === Just tm

standardParseTermTest
  :: (Eq a, Show a)
  => ParseEnv a -> Text -> Term a -> Test
standardParseTermTest env str tm = withFrozenCallStack $ example $
  case runParser (runReaderT standardParser' env) "(test)" str of
    Left err       -> crash $ errorBundlePretty err
    Right parsedTm -> parsedTm === tm

earleyConcreteParseTermTest
  :: ConcreteSyntax -> Text -> Term Void -> Test
earleyConcreteParseTermTest concrete str tm = withFrozenCallStack $ example $
  case tokenizeConcrete (keywords concrete) str of
    Left err     -> crash $ errorBundlePretty err
    Right tokens -> case fullParses (concreteParser concrete) tokens of
      ([parsed], _report) -> parsed === tm
      (_,        report') -> crash $ show report'
