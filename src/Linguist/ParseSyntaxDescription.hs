{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
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
parseOperator = Operator
  <$> parseName
  <*> parseArity
  <*> option "" stringLiteral

parseArity :: Parser Arity
parseArity = fmap Arity $ option [] $ asum
  [ parens $ option [] $ parseValence `sepBy1` symbol ";"

  -- you can elide the parens in the special case of a singleton external
  , fmap (:[]) parseExternal
  ]

parseValence :: Parser Valence
parseValence = asum
  [ parseExternal
  , do
       names <- parseSort `sepBy1` symbol "."
       let Just (sorts, result) = unsnoc names
       pure $ Valence sorts result
  ]

parseSort :: Parser Sort
parseSort = SortAp
  <$> parseName
  <*> many (asum
    [ SortAp <$> parseName <*> pure []
    , parens parseSort
    ])

parseExternal :: Parser Valence
parseExternal = brackets $ External <$> parseName
