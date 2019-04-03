module Lvca.ParseDenotationChart where

import           Data.Foldable   (asum)
import           Data.Text       (Text)
import           Data.Void       (Void)
import           Text.Megaparsec

import           Lvca.Core            (Core)
import           Lvca.DenotationChart
import           Lvca.ParseUtil
import           Lvca.Types


type DenotationChartParser a = Parsec
  Void -- error type
  Text -- stream type
  a

-- TODO: add tagged / untagged options for consistency with standard parser
parseDenotationChart
  :: DenotationChartParser a
  -> DenotationChartParser (DenotationChart a)
parseDenotationChart parseA = do
  _ <- scn -- TODO: principled whitespace handling
  DenotationChart <$> many (parseDenotationLine parseA) <* eof
  <?> "denotation chart"

parseDenotationLine
  :: DenotationChartParser a -> DenotationChartParser (Pattern a, Core)
parseDenotationLine parseA = (,)
  <$> oxfordBrackets (parsePattern parseA)
  <*  symbol "="
  <*> parseDenotationRhs
  <?> "denotation line"

parsePattern
  :: DenotationChartParser a
  -> DenotationChartParser (Pattern a)
parsePattern parseA
  = mkUnion <$> parsePattern' parseA `sepBy1` symbol "|"
  <?> "union of patterns"
  where mkUnion = \case
          [pat] -> pat
          pats  -> PatternUnion pats

parsePattern'
  :: DenotationChartParser a
  -> DenotationChartParser (Pattern a)
parsePattern' parseA = asum
  [ PatternVar Nothing <$ symbol "_" <?> "wildcard pattern"
  , do let betweenSemis = label "binding or term pattern" $ parsePattern parseA

       name <- parseName
       option (PatternVar (Just name)) $ asum
         [ parens $ PatternTm name <$> betweenSemis `sepBy` symbol ";"
         , braces $ PatternPrimVal <$> (asum
             [ Nothing <$  symbol "_" <?> "wildcard pattern"
             , Just    <$> parseA
             ])
         ]
  ] <?> "non-union pattern"

parseBinders :: DenotationChartParser [Text]
parseBinders = try parseName `endBy'` symbol "."
  <?> "binders"

parseDenotationRhs :: DenotationChartParser Core
parseDenotationRhs = undefined

-- asum
--   -- TODO: allow prims
--   -- [ PrimValue . Right <$> braces parseB
--   [ do name <- parseName
--        option (Var name) $ asum
--          [ -- sugar for e.g. `Int{0}`
--            -- do b <- braces parseB
--            --    pure $ Term name [ Scope [] $ PrimValue $ Right b ]
--            parens $ do
--            let boundTerm = do
--                  binders <- parseBinders
--                  tm      <- parseDenotationRhs
--                  pure $ Scope binders tm
--            Term name <$> boundTerm `sepBy` symbol ";"
--          ]
--   , oxfordBrackets $ do
--       name <- parseName
--       pure $ Term "MeaningOf" [ Scope [] $ PrimValue $ Left name ]
--   ] <?> "non-union pattern"
