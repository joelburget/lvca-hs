module Lvca.ParseSyntaxDescription where

import           Control.Lens                  (unsnoc)
import           Data.Foldable                 (asum)
import qualified Data.Map                      as Map
import           Data.Text                     (Text)
import           Data.Void                     (Void)
import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer    as L

import           Lvca.ParseUtil
import           Lvca.Types
import           Text.Megaparsec.Char.LexerAlt (indentBlock)


type SyntaxDescriptionParser a = Parsec
  Void -- error type
  Text -- stream type
  a

parseSyntaxDescription :: SyntaxDescriptionParser SyntaxChart
parseSyntaxDescription
  = SyntaxChart . Map.fromList <$> some parseSortDef <* eof

parseSortDef :: SyntaxDescriptionParser (SortName, SortDef)
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
parseOperator :: SyntaxDescriptionParser Operator
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

parseArity :: SyntaxDescriptionParser Arity
parseArity = fmap Arity $ option [] $
  parens $ option [] $ parseValence `sepBy1` symbol ";"

parseValence :: SyntaxDescriptionParser Valence
parseValence = do
  names <- parseSort `sepBy1` symbol "."
  let Just (sorts, result) = unsnoc names
  pure $ Valence sorts result

parseSort :: SyntaxDescriptionParser Sort
parseSort = asum
  [ braces $ External <$> parseName
  , SortAp
      <$> parseName
      <*> many (asum
        [ SortAp <$> parseName <*> pure []
        , parens parseSort
        ])
  ]
