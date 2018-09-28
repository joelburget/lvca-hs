{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Linguist.ParseDenotationChart where

-- import           Control.Applicative           (($>))
import           Data.Foldable                 (asum)
import           Data.Text                     (Text)
import           Data.Void                     (Void)
import           Text.Megaparsec

import           Linguist.ParseUtil
import           Linguist.Types


type Parser a = Parsec Void Text a

parseDenotationChart :: Parser (DenotationChart a)
parseDenotationChart = DenotationChart <$> many parseDenotationLine <* eof

parseDenotationLine :: Parser (Pattern, Denotation a)
parseDenotationLine = (,)
  <$> oxfordBrackets parsePattern
  <*  symbol "="
  <*> parseDenotationRhs

parsePattern :: Parser Pattern
parsePattern = mkUnion <$> parsePattern' `sepBy1` symbol "|"
  where mkUnion = \case
          [pat] -> pat
          pats  -> PatternUnion pats

-- TODO: how to distinguish primval?
parsePattern' :: Parser Pattern
parsePattern' = asum
  [ PatternVar Nothing <$ symbol "_"
  , do name <- parseName
       option (PatternVar (Just name)) $ asum
         [ symbol "." *> undefined -- BindingPattern
         , parens $ PatternTm name <$> parsePattern `sepBy` symbol ";"
         , brackets $ PatternPrimVar name <$> asum
           [ Nothing <$ symbol "_"
           , Just    <$> parseName
           ]
         ]
  ]

parseDenotationRhs :: Parser (Denotation a)
parseDenotationRhs = undefined
