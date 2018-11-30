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

-- The first two cases are sugar so you can write:
-- - `{Num}` instead of
-- - `Num{Num}` instead of
-- - `Num({Num})`
parseOperator :: Parser Operator
parseOperator = asum
  [ do -- sugar for `{Num}`
       name <- braces parseName
       Operator name (ExternalArity name) <$> option "" stringLiteral
  , do
       name <- parseName
       asum
         [ -- sugar for `Num{Num}`
           Operator name
           <$> braces (ExternalArity <$> parseName)
           <*> option "" stringLiteral
           -- unsweetened
         , Operator name
           <$> parseArity
           <*> option "" stringLiteral
         ]
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
parseSort = asum
  [ braces $ External <$> parseName
  , SortAp
      <$> parseName
      <*> many (asum
        [ SortAp <$> parseName <*> pure []
        , parens parseSort
        ])
  ]
