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

parseSyntaxDescription' :: SyntaxDescriptionParser SyntaxChart
parseSyntaxDescription' = parseSyntaxDescription <* eof

parseSyntaxDescription :: SyntaxDescriptionParser SyntaxChart
parseSyntaxDescription = do
  sortDefs    <- some parseSortDef
  pure $ SyntaxChart (Map.fromList sortDefs)

-- | Parse a sort definition, eg:
--
-- @
-- Foo :=
--   Bar
--   Baz
-- @
parseSortDef :: SyntaxDescriptionParser (SortName, SortDef)
parseSortDef = L.nonIndented scn $ indentBlock scn $ do
  name      <- parseName
  variables <- many parseName
  _         <- symbol ":="

  asum
    -- Try to parse the multiline version, eg:
    --
    -- @
    -- Foo :=
    --   Bar
    --   Baz
    -- @
    [ pure $ L.IndentMany Nothing (pure . (name,) . (SortDef variables))
        parseOperator

    -- TODO:
    -- Failing that, try the single line version:
    --
    -- @
    -- Foo := Bar | Baz
    -- @
    -- , L.IndentNone . (name,) . SortDef variables <$>
    --     parseOperator `sepBy1` symbol "|"
    ]

-- | Parse an operator.
parseOperator :: SyntaxDescriptionParser Operator
parseOperator = do
  name <- parseName
  Operator name <$> parseArity

-- | Parse an arity, which is a list of valences separated by @;@, eg:
--
-- @
-- A; B; C
-- A. B. C
-- A. B. F A B
-- A. B; C
-- A
-- @
parseArity :: SyntaxDescriptionParser Arity
parseArity = fmap FixedArity $ option [] $ -- TODO: variable arity
  parens $ option [] $ parseValence `sepBy1` symbol ";"

-- | Parse a valence, which is a list of sorts separated by @.@, eg any of:
--
-- @
-- A. B. C
-- A. B. F A B
-- A
-- @
parseValence :: SyntaxDescriptionParser Valence
parseValence = do
  names <- parseSort `sepBy1` symbol "."
  let Just (sorts, result) = unsnoc names
  pure $ FixedValence (NamedSort "TODO" <$> sorts) (NamedSort "TODO" result)
  -- TODO: variable valence

-- | Parse a sort, which is a regular sort name, eg:
--
-- @
-- A
-- F A B
-- @
parseSort :: SyntaxDescriptionParser Sort
parseSort = SortAp
  <$> parseName
  <*> many (asum
    [ SortAp <$> parseName <*> pure []
    , parens parseSort
    ])
