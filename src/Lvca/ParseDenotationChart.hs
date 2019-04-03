module Lvca.ParseDenotationChart where

import           Data.Foldable   (asum)
import           Data.Functor    (($>))
import           Data.Text       (Text, pack)
import           Data.Void       (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Lvca.Core (Core(..), Literal(..), Val(..), Var(..), Pat, Ty(..))
import qualified Lvca.Core as Core
import           Lvca.DenotationChart
import           Lvca.ParseUtil
import           Lvca.Types hiding (Var)


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
  <*> parseCore
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

-- This is gross, right?
parsePat :: DenotationChartParser Pat
parsePat = asum
  [ symbol "_" $> Core.PatternDefault
  , Core.PatternLit <$> parseLit
  , do name <- parseName
       -- TODO: remove Maybe?
       option (Core.PatternVar (Just name)) $ parens $
         Core.PatternTm name <$> many parsePat
  ]

parseLit :: DenotationChartParser Literal
parseLit = asum
  [ LitInteger <$> intLiteral
  , LitText <$> stringLiteral
  ]

-- TODO: awkward level of duplication
parseVal :: DenotationChartParser Val
parseVal = asum
  [ do name <- parseName
       parens $ ValTm name <$> many parseVal
  , ValLit <$> parseLit
  , ValPrimop <$> parsePrimop
  -- No lam
  ]

parseCore :: DenotationChartParser Core
parseCore = asum
  [ CoreVal . ValLit <$> parseLit
  , do _ <- symbol "app"
       parens $ do
         f:args <- some parseCore
         pure $ App f args

  , do _ <- symbol "lam"
       parens $ Lam <$> parseBinders <*> parseCore

  , do _ <- symbol "case"
       parens $ do
         scrutinee <- parseCore
         -- TODO: ty

         -- Q:
         branches <- some $ (,) <$> parsePat <*> parseCore
         pure $ Case scrutinee Ty branches

  , CoreVal . ValPrimop <$> parsePrimop

  , do name <- parseName
       option (CoreVar (Var name)) $ parens $
         CoreVal . ValTm name <$> many parseVal

  , Metavar <$> oxfordBrackets parseName
  ] <?> "non-union pattern"

parsePrimop :: DenotationChartParser Text
parsePrimop = pack
  <$> ((:)
       <$> (char '$'
         <?> "variable beginning character")
       <*> many (alphaNumChar <|> char '\'' <|> char '_'
         <?> "variable continuing character")
       <*  scn)
  <?> "primop"
