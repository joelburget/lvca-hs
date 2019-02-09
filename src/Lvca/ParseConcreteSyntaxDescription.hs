module Lvca.ParseConcreteSyntaxDescription where

import           Control.Monad.Reader
import           Data.Foldable                 (asum)
import qualified Data.Sequence                 as Seq
import           Data.Text                     (Text)
import           Data.Traversable              (for)
import           Data.Void                     (Void)
import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer    as L

import           Lvca.ParseTerm                (ParseEnv(..))
import qualified Lvca.ParseTerm                as ParseTerm
import           Lvca.ParseUtil
import           Lvca.Types
import           Text.Megaparsec.Char          (char, eol)
import           Text.Megaparsec.Char.LexerAlt (indentBlock)

type ConcreteSyntaxDescriptionParser a = Parsec
  Void -- error type
  Text -- stream type
  a

-- | Parse a concrete syntax description, eg:
--
-- > concrete syntax:
-- > - Z()       ~ "Z";
-- > - S(x)      ~ "S" x;
-- > - Mul(x; y) ~ infixl x "*" y;
-- > - Add(x; y) ~ infixl x "+" y;
-- >   Sub(x; y) ~ infixl x "-" y;
parseConcreteSyntaxDescription
  :: ParseEnv Void -> ConcreteSyntaxDescriptionParser ConcreteSyntax
parseConcreteSyntaxDescription env = parseConcreteSyntaxDescription' env <* eof

parseConcreteSyntaxDescription'
  :: ParseEnv Void -> ConcreteSyntaxDescriptionParser ConcreteSyntax
parseConcreteSyntaxDescription' env = L.nonIndented scn $ indentBlock scn $ do
  _ <- symbol "concrete syntax:"

  pure $ L.IndentSome (Just (mkPos 3))
    (pure . ConcreteSyntax . Seq.fromList)
    (parsePrecedenceLevel env)

-- | Parse a precedence level, eg:
--
-- > - Add(x; y) ~ infixl x "*" y;
-- >   Sub(x; y) ~ infixl x "-" y;
--
-- or
--
-- > - Let(x; def; body) ~ "let" x "=" def "in" body;
parsePrecedenceLevel
  :: ParseEnv Void
  -> ConcreteSyntaxDescriptionParser [(OperatorName, [Text]) :-> OperatorDirective]
parsePrecedenceLevel env = indentBlock scn $ do
  _ <- symbol "-"
  pure $ L.IndentSome (Just (mkPos 5)) pure $ parsePrecedenceLine env

parsePrecedenceLine
  :: ParseEnv Void
  -> ConcreteSyntaxDescriptionParser ((OperatorName, [Text]) :-> OperatorDirective)
parsePrecedenceLine env = do
  tm <- runReaderT ParseTerm.standardParser' env

  _ <- sc

  (name, subtmNames) <- case tm of
    Term name subtms -> do
      -- each subterm must be just a variable name
      fmap (name,) $ for subtms $ \case
        Var name' -> pure name'
        _         -> fail
          "An abstract syntax operator must be specified with a unique \
          \variable in each subterm slot"
    _ -> fail "Each line in a concrete syntax declaration must be an operator"

  _  <- symbol "~"
  d  <- parseDirective subtmNames <* char ';'
  pure $ (name, subtmNames) :-> d

-- | Parse an infix or mixfix directive
parseDirective :: [Text] -> ConcreteSyntaxDescriptionParser OperatorDirective
parseDirective _
  = parseInfixDirective <|> fmap MixfixDirective parseMixfixDirective

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

-- | Parse a mixfix directive, eg:
--
-- > "let" x "=" y "in" z
parseMixfixDirective :: ConcreteSyntaxDescriptionParser MixfixDirective
parseMixfixDirective = do
  ds <- some $ parseMixfixDirectiveNoSeq <* sc
  case ds of
    [d] -> pure d
    _   -> pure $ foldr1 Sequence ds

parseMixfixDirectiveNoSeq :: ConcreteSyntaxDescriptionParser MixfixDirective
parseMixfixDirectiveNoSeq = asum
  [ Literal <$> stringLiteral
  , Line <$ eol
  -- , TODO Nest
  , do
       _ <- "group"
       parens $ Group <$> parseMixfixDirective
  -- TODO: remove left recursion
  -- , (:<+) <$> parseMixfixDirective <* "<+" <*> parseMixfixDirective
  , SubTerm <$> parseName
  ]
