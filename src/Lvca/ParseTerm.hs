{-# LANGUAGE TemplateHaskell #-}
module Lvca.ParseTerm
  ( Parser
  , ParseEnv(..)
  , ExternalStyle(..)
  , ExternalParser
  , ExternalParsers
  , parseChart
  , parseSort
  , externalStyle
  , standardParser
  , standardParser'
  , makeExternalParsers
  , noExternalParsers
  ) where

import           Control.Lens         hiding (prism)
import           Control.Lens.Extras  (is)
import           Control.Monad.Reader
import           Data.Foldable        (asum, for_, toList)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import qualified Data.Sequence        as Seq
import           Data.Text            (Text, unpack)
import           Data.Void            (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Lvca.ParseUtil
import           Lvca.Types
import           Lvca.Util

-- TODO: we're not actually using Err.
-- TODO: nice error messages
-- newtype Err = Err String
--   deriving (Eq, Ord, IsString)

-- instance ShowErrorComponent Err where
--   showErrorComponent (Err str) = str

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

type ExternalParser  a = Parsec Void Text a
type ExternalParsers a = Map SortName (ExternalParser a)

data ParseEnv a = ParseEnv
  { _parseChart      :: !SyntaxChart
  , _parseSort       :: !Sort
  , _externalStyle   :: !ExternalStyle
  , _externalParsers :: !(ExternalParsers a)
  }
makeLenses ''ParseEnv

type Parser a b = ReaderT (ParseEnv a) (Parsec Void Text) b

-- | Look through the entire syntax description to verify we have all required
-- primParsers.
checkPrimParsers :: Parser a ()
checkPrimParsers = do
  SyntaxChart syntax <- view parseChart
  primParsers        <- view externalParsers

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

-- | Parse a term using the standard syntax, consuming the entire file.
standardParser' :: forall a. Parser a (Term a)
standardParser' = standardParser <* eof

-- | Generates a parser for any language parsing a standard syntax. Example:
--
-- > Add(
-- >   Times(1; 2);
-- >   Ap(
-- >     Lam(x. Times(x; x));
-- >     3
-- >   )
-- > )
standardParser :: forall a. Parser a (Term a)
standardParser = do
  SyntaxChart syntax <- view parseChart
  primParsers        <- view externalParsers

  checkPrimParsers

  let sortParsers :: Map SortName (Map Text Sort -> Parser a (Term a))
      sortParsers = syntax <@&> \sortName (SortDef _vars ops) concreteSorts ->

        -- build a parser for each operator in this sort
        let opParsers = ops <&> \case
              -- Sugar for operators containing just an external:
              -- - If we're using tagged externals, optionally allow elision of
              --   parens
              -- - If we're using untagged externals, parse the external here
              Operator name (ExternalArity name')
                | name == name' -> do
                  externalStyle' <- view externalStyle

                  case primParsers ^? ix name of
                    Nothing -> fail "TODO"
                    Just parsePrim ->
                      let parsePrim' = ReaderT (const parsePrim)
                      in case externalStyle' of
                           UntaggedExternals ->
                             Term name . (:[]) . Scope [] . PrimValue
                               <$> parsePrim'
                           TaggedExternals   -> do
                             _    <- string name
                             prim <- braces parsePrim'
                                 <|> parens (braces parsePrim')
                             pure $ Term name [ Scope [] $ PrimValue prim ]

              Operator name (FixedArity valences') -> label (unpack name) $ do
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

  parseTerm

parseValence :: Parser a (Term a) -> Valence -> Parser a (Scope a)
parseValence parseTerm valence@(FixedValence sorts bodySort) = do
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

  label ("valence " <> show valence) $ Scope
    <$> countSepBy (length sorts) parseName (symbol ".")
    <*> parseTerm'

noExternalParsers :: ExternalParsers Void
noExternalParsers = Map.empty

makeExternalParsers :: [(SortName, ExternalParser a)] -> ExternalParsers a
makeExternalParsers = Map.fromList
