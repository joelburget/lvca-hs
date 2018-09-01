{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections          #-}
module Linguist.ParseSyntaxDescription where

import Data.Void (Void)
import Data.Foldable (asum)
import qualified Data.Map as Map
import Control.Lens (unsnoc)
import Data.Text (Text, pack)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Linguist.Types
import           Linguist.ParseUtil
import           Text.Megaparsec.Char.LexerAlt (indentBlock)


type Parser a = Parsec Void Text a

parseSyntaxDescription :: Parser SyntaxChart
parseSyntaxDescription = SyntaxChart . Map.fromList <$> some parseSort <* eof

parseSort :: Parser (SortName, Sort)
parseSort = L.nonIndented scn $ indentBlock scn $ do
  name      <- parseName
  _         <- symbol "::="
  -- pos <- L.indentLevel
  -- TODO: get rid of sort variables
  pure $ L.IndentMany Nothing (pure . (name,) . (Sort [])) parseOperator

parseOperator :: Parser Operator
parseOperator = Operator
  <$> parseName
  <*> parseArity
  <*> option "" stringLiteral

-- TODO: support parsing externals
parseArity :: Parser Arity
parseArity = parens (asum
  [ Arity <$> parseValence `sepBy1` symbol ";"
  -- , External <$>
  , Arity [] <$ ""
  ]) <|> Arity [] <$ ""

parseValence :: Parser Valence
parseValence = do
  names <- parseName `sepBy1` symbol "."
  let Just (sorts, result) = unsnoc names
  pure $ Valence sorts result

-- TODO: deduplicate with parseVarName
parseName :: Parser Text
parseName = pack <$> ((:) <$> letterChar <*> many alphaNumChar <* space)
  <?> "sort name"
