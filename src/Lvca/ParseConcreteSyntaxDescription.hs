module Lvca.ParseConcreteSyntaxDescription where

import           Data.Foldable                 (asum)
import           Data.List                     (elem)
import qualified Data.Sequence                 as Seq
import           Data.Text                     (Text)
import           Data.Traversable              (for)
import           Data.Void                     (Void)
import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer    as L

-- import           Lvca.ParseDenotationChart     (parsePattern)
import           Lvca.ParseUtil
import           Lvca.Types                    hiding (Scope)
import           Text.Megaparsec.Char          (char, eol)
import           Text.Megaparsec.Char.LexerAlt (indentBlock)

type ConcreteSyntaxDescriptionParser a = Parsec
  Void -- error type
  Text -- stream type
  a

parseConcreteSyntaxDescription'
  :: ConcreteSyntaxDescriptionParser ConcreteSyntax
parseConcreteSyntaxDescription' = parseConcreteSyntaxDescription <* eof

-- | Parse a concrete syntax description, eg:
--
-- > - Z()       ~ "Z";
-- > - S(x)      ~ "S" x;
-- > - Mul(x; y) ~ infixl x "*" y;
-- > - Add(x; y) ~ infixl x "+" y;
-- >   Sub(x; y) ~ infixl x "-" y;
parseConcreteSyntaxDescription
  :: ConcreteSyntaxDescriptionParser ConcreteSyntax
parseConcreteSyntaxDescription = do
  levels <- some parsePrecedenceLevel
  pure $ ConcreteSyntax $ Seq.fromList levels

-- | Parse a precedence level, eg:
--
-- > - Add(x; y) ~ infixl x "*" y;
-- >   Sub(x; y) ~ infixl x "-" y;
--
-- or
--
-- > - Let(x; def; body) ~ "let" x "=" def "in" body;
parsePrecedenceLevel :: ConcreteSyntaxDescriptionParser [ConcreteSyntaxRule]
parsePrecedenceLevel = indentBlock scn $ do
  _ <- symbol "-"
  pure $ L.IndentSome (Just (mkPos 3)) pure parsePrecedenceLine

-- TODO: problems with duplication
data Pat
  = PatTm !Text ![Scope]
  | PatVar !(Maybe Text)
  deriving Show

data Scope = Scope ![Text] !Pat
  deriving Show

parsePattern :: ConcreteSyntaxDescriptionParser Pat
parsePattern = asum
  [ PatVar Nothing <$ symbol "_" <?> "wildcard pattern"
  , do let parseScope = label "binding or term pattern" $ do
             binders <- parseName `endBy'` symbol "."
             body    <- parsePattern
             pure $ Scope binders body

       name <- parseName
       option (PatVar (Just name)) $ parens $ do
         PatTm name <$> parseScope `sepBy` symbol ";"
  ] <?> "non-union pattern"

-- | Parse a line, eg:
--
-- > Sub(x; y) ~ infixl x "-" y;
parsePrecedenceLine :: ConcreteSyntaxDescriptionParser ConcreteSyntaxRule
parsePrecedenceLine = do
  pat <- parsePattern
  _   <- sc

  (name, slots) <- case pat of
    PatTm name subpats -> fmap (name,) $
      for subpats $ \(Scope binders tm) -> case tm of
        -- each subterm must be just a variable name
        PatVar (Just name') -> pure (binders, name')
        _                   -> fail
          "An abstract syntax operator must be specified with a unique \
          \variable in each subterm slot"
    x -> fail $
      "Each line in a concrete syntax declaration must be an operator (got " ++
      -- TODO: pretty
      show x ++ ")"

  _ <- symbol "~"

  -- TODO: check uniqueness of all variables
  let subtmNames = snd <$> slots
  d <- parseDirective subtmNames <* char ';'
  pure $ ConcreteSyntaxRule name slots d

-- | Parse an infix or mixfix directive
parseDirective :: [Text] -> ConcreteSyntaxDescriptionParser OperatorDirective
parseDirective subtmNames
  = parseInfixDirective <|>
    parseAssocDirective <|>
    fmap MixfixDirective (parseMixfixDirective subtmNames)

-- | Parse an infix directive, eg:
--
-- > infixl x "*" y
parseInfixDirective :: ConcreteSyntaxDescriptionParser OperatorDirective
parseInfixDirective = do
  fixity <- asum
    [ Infixl <$ "infixl"
    , Infixr <$ "infixr"
    , Infix  <$ "infix"
    ] <* sc

  _   <- parseName
  str <- stringLiteral <* sc
  _   <- parseName

  pure $ InfixDirective str fixity

-- | Parse an assoc directive, eg:
--
-- > assocl f g
parseAssocDirective :: ConcreteSyntaxDescriptionParser OperatorDirective
parseAssocDirective = do
  fixity <- asum
    [ Assocl <$ "assocl"
    , Assocr <$ "assocr"
    ] <* sc

  _   <- parseName
  _   <- parseName

  pure $ AssocDirective fixity

-- | Parse a mixfix directive, eg:
--
-- > "let" x "=" y "in" z
parseMixfixDirective
  :: [Text] -> ConcreteSyntaxDescriptionParser MixfixDirective
parseMixfixDirective subtmNames = do
  ds <- some $ parseMixfixDirectiveNoSeq subtmNames <* sc
  case ds of
    [d] -> pure d
    _   -> pure $ foldr1 Sequence ds

parseMixfixDirectiveNoSeq
  :: [Text] -> ConcreteSyntaxDescriptionParser MixfixDirective
parseMixfixDirectiveNoSeq subtmNames = asum
  [ Literal <$> stringLiteral
  , Line <$ eol
  -- , TODO Nest
  , do
       _ <- "group"
       parens $ Group <$> parseMixfixDirective subtmNames
  -- TODO: remove left recursion
  -- , (:<+) <$> parseMixfixDirective <* "<+" <*> parseMixfixDirective
  , do name <- parseName
       pure $ if name `elem` subtmNames
         then SubTerm name
         else VarName name
  ]
