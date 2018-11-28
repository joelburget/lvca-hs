{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
module Linguist.ParseLanguage
  ( Parser
  , ParseEnv(..)
  , ExternalStyle(..)
  , ExternalParsers
  , parseChart
  , parseSort
  , externalStyle
  , standardParser
  , makeExternalParsers
  , noExternalParsers
  , prop_parse_pretty
  ) where

import           Control.Lens
import           Control.Lens.Extras  (is)
import           Control.Monad.Reader
import           Data.Foldable        (asum, toList, for_)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import qualified Data.Sequence        as Seq
import           Data.String          (IsString (fromString))
import           Data.Text            (Text, unpack)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Hedgehog             hiding (Var)
import           Data.Text.Prettyprint.Doc             (defaultLayoutOptions,
                                                        layoutPretty, Pretty(pretty))
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           Data.Void            (Void)

import           Linguist.ParseUtil
import           Linguist.Types
import           Linguist.Util


-- TODO: we're not actually using Err.
-- TODO: nice error messages
newtype Err = Err String
  deriving (Eq, Ord, IsString)

instance ShowErrorComponent Err where
  showErrorComponent (Err str) = str

-- | There are two ways to parse externals:
--
--   [@TaggedExternals@] Example: @Plus(1; Length("foo"))@. This tries the
--   external parsers in the order specified in the syntax description.
--
--   [@UntaggedExternals@] Example: @Plus(Int{1}; Length(Str{"foo"}))@. This
--   looks for an exact match for one of the provided parsers.
data ExternalStyle
  = TaggedExternals
  | UntaggedExternals

data ParseEnv = ParseEnv
  { _parseChart    :: !SyntaxChart
  , _parseSort     :: !SortName
  , _externalStyle :: !ExternalStyle
  }
makeLenses ''ParseEnv

type Parser a = ReaderT ParseEnv (Parsec Err Text) a

type ExternalParsers a = Map SortName (Map Text (Parser a))

-- | Generates a parser for any language parsing a standard syntax. Example
--
-- @
--     Add(
--       Times(1; 2);
--       Ap(
--         Lam(x. Times(x; x));
--         3
--       )
--     )
-- @
standardParser
  :: forall a. Map SortName (Map Text (Parser a)) -> Parser (Term a)
standardParser primParsers = do
  ParseEnv (SyntaxChart syntax) _ externalStyle' <- ask

  ifor_ syntax $ \sortName (SortDef _ operators) ->
    for_ (operators ^.. traverse . filtered (is _External)) $ \external ->
      let opName :: Text
          opName = external ^. operatorName
      in case primParsers ^? ix sortName . ix opName of
           Just _  -> pure ()
           Nothing -> fail $ "The set of provided external parsers doesn't match the set of externals specified in the syntax description (in particular, we're missing a parser for " <> unpack sortName <> ":" <> unpack opName <> "). Please specify all parsers (hint: use `noParse` for cases where the external should not be parsed at all)"

  let sortParsers :: Map SortName (Parser (Term a))
      sortParsers = syntax <@&> \sortName (SortDef _vars operators) ->

        -- build a parser for each operator in this sort
        let opParsers = operators <&> \case
              Operator name (Arity valences) _ ->
                label (unpack name) $ do
                  _ <- try $ do
                    _ <- string $ fromString $ unpack name
                    notFollowedBy $ alphaNumChar <|> char '\'' <|> char '_'
                  subTms <- parens $ case valences of
                    []   -> pure Seq.empty
                    v:vs -> foldl
                      (\parseLeft valence -> do
                        pvs <- parseLeft
                        _   <- symbol ";"
                        pv  <- parseValence parseTerm valence
                        pure $ pvs |> pv)
                      (Seq.singleton <$> parseValence parseTerm v)
                      vs
                  -- TODO: convert Term to just use Sequence
                  pure $ Term name $ toList subTms

              External name _desc -> do
                let Just parsePrim = primParsers ^? ix sortName . ix name
                case externalStyle' of
                  TaggedExternals
                    -> PrimValue <$> string name <*> braces parsePrim
                  UntaggedExternals -> PrimValue name <$> parsePrim

        in asum opParsers <?> (unpack sortName ++ " operator")

      -- parse an operator in the current sort or a variable
      parseTerm = do
        sort <- view parseSort
        case sortParsers ^? ix sort of
          Just opParsers -> opParsers <|> fmap Var parseName
          Nothing -> fail $
            "unable to find sort " <> unpack sort <> " among " <>
            -- TODO: more user-friendly show
            show (Map.keys sortParsers)

  parseTerm <* eof

parseValence
  :: Parser (Term a)
  -> Valence
  -> Parser (Term a)
parseValence parseTerm valence@(Valence sorts (SortAp resultSort _))
  = label ("valence " <> show valence)
  $ local (parseSort .~ resultSort)
  $ if null sorts
    then parseTerm
    else Binding
      <$> countSepBy (length sorts) parseName (symbol ".")
      <*> local (parseSort .~ resultSort) parseTerm

prop_parse_pretty
  :: (Show a, Pretty a, Eq a)
  => SyntaxChart
  -> SortName
  -> (SortName -> Maybe (Gen a))
  -> Map SortName (Map Text (Parser a))
  -> Property
prop_parse_pretty chart sort aGen aParsers = property $ do
  tm <- forAll $ genTerm chart sort aGen
    -- (Just (Gen.int Range.exponentialBounded))

  let pretty' = renderStrict . layoutPretty defaultLayoutOptions . pretty
      parse'  = parseMaybe (runReaderT (standardParser aParsers)
        (ParseEnv chart sort TaggedExternals))

  annotate $ unpack $ pretty' tm
  parse' (pretty' tm) === Just tm

noExternalParsers :: ExternalParsers Void
noExternalParsers = Map.empty

makeExternalParsers :: [(SortName, [(Text, Parser a)])] -> ExternalParsers a
makeExternalParsers = Map.fromList . fmap (fmap Map.fromList)
