module Linguist.ParseSyntaxDescription where

import           Control.Lens                  (unsnoc)
import           Data.Foldable                 (asum)
import qualified Data.Map                      as Map
import           Data.Text                     (Text)
import           Data.Void                     (Void)
import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer    as L

import           Linguist.ParseUtil
import           Linguist.Types
import           Text.Megaparsec.Char.LexerAlt (indentBlock)


type Parser a = Parsec Void Text a

parseSyntaxDescription :: Parser SyntaxChart
parseSyntaxDescription
  = SyntaxChart . Map.fromList <$> some parseSortDef <* eof

parseSortDef :: Parser (SortName, SortDef)
parseSortDef = L.nonIndented scn $ indentBlock scn $ do
  name      <- parseName
  variables <- many parseName
  _         <- symbol "::="
  pure $ L.IndentMany Nothing (pure . (name,) . (SortDef variables))
    parseOperator

parseOperator :: Parser Operator
parseOperator = asum
  [ Operator
    <$> parseName
    <*> parseArity
    <*> option "" stringLiteral
  , External
    <$> braces (parseName)
    <*> option "" stringLiteral
  ]

parseArity :: Parser Arity
parseArity = fmap Arity $ option [] $
  parens $ option [] $ parseValence `sepBy1` symbol ";"

parseValence :: Parser Valence
parseValence = do
  names <- parseSort `sepBy1` symbol "."
  let Just (sorts, result) = unsnoc names
  pure $ Valence sorts result

parseSort :: Parser Sort
parseSort = SortAp
  <$> parseName
  <*> many (asum
    [ SortAp <$> parseName <*> pure []
    , parens parseSort
    ])
