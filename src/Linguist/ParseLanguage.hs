{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
module Linguist.ParseLanguage
  ( Parser
  , ParseEnv(..)
  , ExternalStyle(..)
  , ExternalParser
  , ExternalParsers
  , parseChart
  , parseSort
  , externalStyle
  , standardParser
  , makeExternalParsers
  , noExternalParsers
  , prop_parse_pretty
  , parseTest
  ) where

import           Control.Lens
import           Control.Lens.Extras  (is)
import           Control.Monad.Reader
import           Data.Foldable        (asum, toList, for_)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import qualified Data.Sequence        as Seq
import           Data.String          (IsString)
import           Data.Text            (Text, unpack)
import           Text.Megaparsec      hiding (parseTest)
import           Text.Megaparsec.Char
import           Hedgehog             hiding (Test, Var)
import           Data.Text.Prettyprint.Doc             (defaultLayoutOptions,
                                                        layoutPretty, Pretty(pretty))
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           Data.Void            (Void)
import           EasyTest             (Test, expectEq)

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

type ExternalParser  a = Parsec Err Text a
type ExternalParsers a = Map SortName (ExternalParser a)

data ParseEnv a = ParseEnv
  { _parseChart      :: !SyntaxChart
  , _parseSort       :: !Sort
  , _externalStyle   :: !ExternalStyle
  , _externalParsers :: !(ExternalParsers a)
  }
makeLenses ''ParseEnv

type Parser a b = ReaderT (ParseEnv a) (Parsec Err Text) b

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
standardParser :: forall a. Parser a (Term a)
standardParser = do
  SyntaxChart syntax <- view parseChart
  primParsers        <- view externalParsers

  -- Look through the entire syntax description to verify we have all required
  -- primParsers.
  --
  -- Example:
  --
  -- > Block ::= Header(HeaderLevel; {Text})
  -- > ^ traverse (each sort)
  -- >           ^ sortOperators
  -- >                 ^ operatorArity
  -- >                  ^            ^ valences . traverse
  -- >                  ^            ^ valenceSorts
  -- >                               ^ filtered (is _External)
  -- >                                ^ externalName
  for_ (syntax
    ^.. traverse
      . sortOperators
      . traverse
      . operatorArity
      . valences
      . traverse
      . valenceSorts
      . filtered (is _External)
      . externalName) $ \name ->
    case primParsers ^? ix name of
      Just _  -> pure ()
      Nothing -> fail $ "The set of provided external parsers doesn't match \
        \the set of externals specified in the syntax description (in \
        \particular, we're missing a parser for (at least) " <> unpack name <>
        "). Please specify all parsers (hint: use `noParse` for cases where \
        \the external should not be parsed at all)"

  let sortParsers :: Map SortName (Map Text Sort -> Parser a (Term a))
      sortParsers = syntax <@&> \sortName (SortDef _vars ops) concreteSorts ->

        -- build a parser for each operator in this sort
        let opParsers = ops <&> \case
              -- Sugar for operators containing just an external:
              -- - If we're using tagged externals, optionally allow elision of
              --   parens
              -- - If we're using untagged externals, parse the external here
              Operator name (ExternalArity name') _desc
                | name == name' -> do
                  externalStyle' <- view externalStyle

                  case primParsers ^? ix name of
                    Nothing -> fail "TODO"
                    Just parsePrim ->
                      let parsePrim' = ReaderT (const parsePrim)
                      in case externalStyle' of
                           UntaggedExternals ->
                             Term name . (:[]) . PrimValue <$> parsePrim'
                           TaggedExternals   -> do
                             _    <- string name
                             prim <- braces parsePrim'
                                 <|> parens (braces parsePrim')
                             pure $ Term name [ PrimValue prim ]

              Operator name (Arity valences') _ -> label (unpack name) $ do
                _ <- try $ do
                  _ <- string name
                  notFollowedBy $ alphaNumChar <|> char '\'' <|> char '_'

                let valences'' = valenceSubst concreteSorts <$> valences'

                subTms <- parens $ case valences'' of
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

        in asum opParsers <?> unpack sortName ++ " operator"

      -- parse an operator in the current sort or a variable
      parseTerm = do
        SortAp sortHead sortArgs <- view parseSort
        let SortDef sortVars _ = syntax ^?! ix sortHead
            sortVarVals = Map.fromList (zip sortVars sortArgs)

        case sortParsers ^? ix sortHead of
          Just sortOpParsers
            -> sortOpParsers sortVarVals <|> fmap Var parseName
          Nothing -> fail $
            "unable to find sort " <> unpack sortHead <> " among " <>
            show (Map.keys sortParsers)

  parseTerm <* eof

parseValence :: Parser a (Term a) -> Valence -> Parser a (Term a)
parseValence parseTerm valence@(Valence sorts bodySort) = do
  primParsers <- view externalParsers
  let parseTerm' = case bodySort of
        External name -> do
          externalStyle' <- view externalStyle
          let Just parsePrim = primParsers ^? ix name
              parsePrim' = ReaderT (const parsePrim)
          case externalStyle' of
            TaggedExternals   -> PrimValue <$> braces parsePrim'
            UntaggedExternals -> PrimValue <$> parsePrim'
        SortAp _ _ -> local (parseSort .~ bodySort) parseTerm


  label ("valence " <> show valence) $
    if null sorts
    then parseTerm'
    else Binding
      <$> countSepBy (length sorts) parseName (symbol ".")
      <*> parseTerm'

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

noExternalParsers :: ExternalParsers Void
noExternalParsers = Map.empty

makeExternalParsers :: [(SortName, ExternalParser a)] -> ExternalParsers a
makeExternalParsers = Map.fromList

parseTest
  :: (Eq a, Show a)
  => ParseEnv a -> Text -> Term a -> Test ()
parseTest env str tm =
  case runParser (runReaderT standardParser env) "(test)" str of
    Left err       -> fail $ errorBundlePretty err
    Right parsedTm -> expectEq parsedTm tm
