module Linguist.ParseDenotationChart where

-- import           Control.Applicative           (($>))
import Control.Monad
import Control.Lens (review)
import           Data.Foldable                 (asum)
import           Data.Text                     (Text)
import           Data.Void                     (Void)
import           Text.Megaparsec
import           Data.Diverse.Lens.Which

import           Linguist.ParseUtil
import           Linguist.Types


type Parser a = Parsec Void Text a

parseDenotationChart
  :: AsFacet Text b => Parser a -> Parser b -> Parser (DenotationChart a b)
parseDenotationChart parseA parseB = do
  _ <- scn -- TODO: principled whitespace handling
  DenotationChart <$> many (parseDenotationLine parseA parseB) <* eof
  <?> "denotation chart"

parseDenotationLine
  :: AsFacet Text b => Parser a -> Parser b -> Parser (Pattern a, Term b)
parseDenotationLine parseA parseB = (,)
  <$> oxfordBrackets (parsePattern parseA)
  <*  symbol "="
  <*> parseDenotationRhs parseB
  <?> "denotation line"

parsePattern :: Parser a -> Parser (Pattern a)
parsePattern parseA
  = mkUnion <$> parsePattern' parseA `sepBy1` symbol "|"
  <?> "union of patterns"
  where mkUnion = \case
          [pat] -> pat
          pats  -> PatternUnion pats

parsePattern' :: Parser a -> Parser (Pattern a)
parsePattern' parseA = asum
  [ PatternVar Nothing <$ symbol "_" <?> "wildcard pattern"
  , do let betweenSemis = label "binding or term pattern" $ do
             binders <- parseBinders
             body    <- parsePattern parseA
             pure $ case binders of
               [] -> body
               _  -> BindingPattern binders body

       name <- parseName
       option (PatternVar (Just name)) $
         parens $ PatternTm name <$> betweenSemis `sepBy` symbol ";"
  , PatternPrimVal <$> braces parseA
  ] <?> "non-union pattern"

endBy' :: (MonadPlus m, MonadParsec e s m) => m a -> m sep -> m [a]
endBy' p sep = many $ try $ do
  x <- p
  re x sep
{-# INLINE endBy' #-}

re :: Monad m => a -> m b -> m a
re x = liftM (const x)
{-# INLINE re #-}

parseBinders :: Parser [Text]
parseBinders = try parseName `endBy'` symbol "."
  <?> "binders"

parseDenotationRhs :: AsFacet Text b => Parser b -> Parser (Term b)
parseDenotationRhs parseB = asum
  [ do name <- parseName
       option (Var name) $ parens $ do
         let boundTerm = do
               binders <- parseBinders
               tm      <- parseDenotationRhs parseB
               pure $ case binders of
                 [] -> tm
                 _  -> Binding binders tm
         Term name <$> boundTerm `sepBy` symbol ";"
  , do meaningVar <- oxfordBrackets $
         Term "MeaningPatternVar" . (:[]) . PrimValue . review (facet @Text) <$>
         parseName
       option meaningVar $ brackets $ do
         to   <- parseName
         _    <- symbol "/"
         from <- parseName
         pure $ Term "Renaming"
           [ PrimValue $ review (facet @Text) from
           , PrimValue $ review (facet @Text) to
           , meaningVar
           ]
  , PrimValue <$> braces parseB
  ] <?> "non-union pattern"
