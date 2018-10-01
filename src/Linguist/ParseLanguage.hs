{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module Linguist.ParseLanguage where

import           Control.Applicative  ((<$))
import           Control.Lens
import           Control.Monad.Reader
import           Data.Foldable        (asum, toList)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import qualified Data.Sequence        as Seq
import           Data.String          (IsString (fromString))
import           Data.Text            (Text, unpack)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Linguist.ParseUtil
import           Linguist.Types
import           Linguist.Util

-- TODO: we're not actually using Err.
-- TODO: nice error messages
newtype Err = Err String
  deriving (Eq, Ord, IsString)

instance ShowErrorComponent Err where
  showErrorComponent (Err str) = str

-- data ParseEnv = ParseEnv
--   { _parseChart :: !SyntaxChart
--   , _parseSort  :: !SortName
--   }

type ParseEnv = (SyntaxChart, SortName)

type Parser a = ParsecT Err Text (Reader ParseEnv) a

parseChart :: Lens' ParseEnv SyntaxChart
parseChart = _1

parseSort :: Lens' ParseEnv SortName
parseSort = _2

standardParser :: forall a. Parser a -> Parser (Term a)
standardParser parsePrim = do
  SyntaxChart syntax <- view parseChart
  let sortParsers :: Map SortName (Parser (Term a))
      sortParsers = syntax <@&> \sortName (SortDef _vars operators) ->
        let opParsers = operators <&> \(Operator name arity _desc) -> case arity of
              -- External sort -> PrimValue <$> local (parseSort .~ sort) parsePrim
              Arity valences -> (do
                _ <- string $ fromString $ unpack name
                subTms <- case valences of
                  [] -> Seq.empty <$ optional (symbol "()")
                  v:vs -> parens $ foldl
                    (\parseLeft valence -> do
                      pvs <- parseLeft
                      _   <- symbol ";"
                      pv  <- parseValence parsePrim parseTerm valence
                      pure $ pvs |> pv)
                    (Seq.singleton <$> parseValence parsePrim parseTerm v)
                    vs
                -- TODO: convert Term to just use Sequence
                pure $ Term name $ toList subTms) <?> unpack name
        in asum opParsers <?> unpack sortName
      parseTerm = do
        sort <- view parseSort
        case sortParsers ^? ix sort of
          Just opParsers -> opParsers <|> fmap Var parseName
          Nothing -> fail $
            "unable to find sort " <> unpack sort <> " among " <>
            -- TODO: more user-friendly show
            show (Map.keys sortParsers)
  parseTerm

parseValence
  :: Parser a
  -> Parser (Term a)
  -> Valence
  -> Parser (Term a)
parseValence _parsePrim parseTerm valence@(Valence sorts (SortAp resultSort _))
  -- TODO: more user-friendly showing of valence
  | null sorts = local (parseSort .~ resultSort) parseTerm
    <?> "valence " <> show valence
  | otherwise = Binding
    <$> countSepBy (length sorts) parseName (symbol ".")
    <*> local (parseSort .~ resultSort) parseTerm
    <?> "valence " <> show valence
parseValence parsePrim _parseTerm (External name) =
  -- TODO: do we need this local?
  PrimValue <$> local (parseSort .~ name) parsePrim
