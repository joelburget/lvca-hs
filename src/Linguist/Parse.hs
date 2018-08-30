{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module Linguist.Parse where

import           Control.Applicative        ((<$))
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader
import           Data.Foldable              (asum, toList)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import qualified Data.Sequence              as Seq
import           Data.String                (IsString (fromString))
import           Data.Text                  (Text, pack, unpack)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Linguist.Types
import           Linguist.Util

-- TODO: we're not actually using Err.
newtype Err = Err String
  deriving (Eq, Ord, IsString)

instance ShowErrorComponent Err where
  showErrorComponent (Err str) = str

-- data ParseEnv = ParseEnv
--   { _parseChart :: !SyntaxChart
--   , _parseSort  :: !SortName
--   }

type ParseEnv = (SyntaxChart, SortName)

parseChart :: Lens' ParseEnv SyntaxChart
parseChart = _1

parseSort :: Lens' ParseEnv SortName
parseSort = _2

type Parser a = ParsecT Err Text (Reader ParseEnv) a

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

standardParser :: forall a. Parser a -> Parser (Term a)
standardParser parsePrim = do
  SyntaxChart syntax <- view parseChart
  let sortParsers :: Map SortName (Parser (Term a))
      sortParsers = syntax <@&> \sortName (Sort _vars operators) ->
        let opParsers = operators <&> \(Operator name arity _desc) -> case arity of
              External sort -> PrimValue <$> local (parseSort .~ sort) parsePrim
              Arity valences -> (do
                _ <- string $ fromString $ unpack name
                subTms <- case valences of
                  [] -> Seq.empty <$ optional (symbol "()")
                  v:vs -> parens $ foldl
                    (\parseLeft valence -> do
                      pvs <- parseLeft
                      _   <- symbol ";"
                      pv  <- parseValence parseTerm valence
                      pure $ pvs |> pv)
                    (Seq.singleton <$> parseValence parseTerm v)
                    vs
                -- TODO: convert Term to just use Sequence
                pure $ Term name $ toList subTms) <?> unpack name
        in asum opParsers <?> unpack sortName
      parseTerm = do
        sort <- view parseSort
        case sortParsers ^? ix sort of
          Just opParsers -> opParsers <|> fmap Var parseVarName
          Nothing -> fail $
            "unable to find sort " <> unpack sort <> " among " <>
            -- TODO: more user-friendly show
            show (Map.keys sortParsers)
  parseTerm

parseVarName :: Parser Text
parseVarName = pack <$> ((:) <$> letterChar <*> many alphaNumChar)
  <?> "variable"

parseValence
  :: Parser (Term a)
  -> Valence
  -> Parser (Term a)
parseValence parseTerm valence@(Valence sorts resultSort)
  -- TODO: more user-friendly showing of valence
  | null sorts = local (parseSort .~ resultSort) parseTerm
    <?> "valence " <> show valence
  | otherwise = Binding
    <$> countSepBy (length sorts) parseVarName (symbol ".")
    <*> local (parseSort .~ resultSort) parseTerm
    <?> "valence " <> show valence

countSepBy :: MonadPlus m => Int -> m a -> m sep -> m [a]
countSepBy 0 _ _ = pure []
countSepBy n ma sep = do
  a  <- ma
  _  <- sep
  as <- countSepBy (pred n) ma sep
  pure (a:as)
