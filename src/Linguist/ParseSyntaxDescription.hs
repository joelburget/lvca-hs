{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections          #-}
module Linguist.ParseSyntaxDescription where

import Data.Void (Void)
import Data.Foldable (asum)
import qualified Data.Map as Map
import Control.Lens (unsnoc)
import Data.Text (Text)
import           Text.Megaparsec
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
  -- TODO: get rid of sort variables
  pure $ L.IndentMany Nothing (pure . (name,) . (Sort [])) parseOperator

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
       names <- parseName `sepBy1` symbol "."
       let Just (sorts, result) = unsnoc names
       pure $ Valence sorts result
  ]

parseExternal :: Parser Valence
parseExternal = brackets $ External <$> parseName
