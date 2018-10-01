module Linguist.ParseDenotationChart where

-- import           Control.Applicative           (($>))
import           Data.Foldable                 (asum)
import           Data.Text                     (Text)
import           Data.Void                     (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char (eol)

import           Linguist.ParseUtil
import           Linguist.Types


type Parser a = Parsec Void Text a

parseDenotationChart :: Parser a -> Parser b -> Parser (DenotationChart a b)
parseDenotationChart parseA parseB
  = DenotationChart <$> many (parseDenotationLine parseA parseB) <* eof
  <?> "denotation chart"

parseDenotationLine :: Parser a -> Parser b -> Parser (Pattern a, Term b)
parseDenotationLine parseA parseB = (,)
  <$> oxfordBrackets (parsePattern parseA)
  <*  symbol "="
  <*> parseDenotationRhs parseB
  <*  eol
  <?> "denotation line"

parsePattern :: Parser a -> Parser (Pattern a)
parsePattern parseA = mkUnion <$> parsePattern' parseA `sepBy1` symbol "|"
                    <?> "union of patterns"
  where mkUnion = \case
          [pat] -> pat
          pats  -> PatternUnion pats

parsePattern' :: Parser a -> Parser (Pattern a)
parsePattern' parseA = asum
  [ PatternVar Nothing <$ symbol "_" <?> "wildcard pattern"
  , do name <- parseName
       option (PatternVar (Just name)) $ asum
         [ symbol "." *> error "TODO" -- BindingPattern
           <?> "binding pattern"
         , parens (PatternTm name <$> parsePattern parseA `sepBy` symbol ";")
           <?> "term pattern"
         ]
  , PatternPrimVal <$> brackets parseA
  ] <?> "non-union pattern"

parseDenotationRhs :: Parser b -> Parser (Term b)
parseDenotationRhs parseB = asum
  [ do name <- parseName
       option (Var name) $
         -- TODO: how to do binding?
         parens (Term name <$> parseDenotationRhs parseB `sepBy` symbol ";")
  , oxfordBrackets $ do
       name <- parseName
       -- pure $ Term "denotation" [Var name]
       pure $ Var name
  -- TODO: change this to braces?
  , PrimValue <$> brackets parseB
  ] <?> "non-union pattern"
