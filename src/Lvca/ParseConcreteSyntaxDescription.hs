module Lvca.ParseConcreteSyntaxDescription where

-- import           Control.Lens                  (unsnoc)
-- import           Data.Foldable                 (asum)
import qualified Data.Sequence                 as Seq
import           Data.Text                     (Text)
import           Data.Void                     (Void)
import           Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer    as L

import           Lvca.ParseUtil
import           Lvca.Types
import           Text.Megaparsec.Char.LexerAlt (indentBlock)


type ConcreteSyntaxDescriptionParser a = Parsec
  Void -- error type
  Text -- stream type
  a

-- newtype ConcreteSyntax = ConcreteSyntax
--   (Seq [OperatorName :-> OperatorDirective])

parseConcreteSyntaxDescription
  :: ConcreteSyntaxDescriptionParser ConcreteSyntax
parseConcreteSyntaxDescription = parseConcreteSyntaxDescription' <* eof

parseConcreteSyntaxDescription'
  :: ConcreteSyntaxDescriptionParser ConcreteSyntax
parseConcreteSyntaxDescription' = L.nonIndented scn $ indentBlock scn $ do
  _ <- symbol "concrete syntax:"

  pure $ L.IndentSome Nothing
    (pure . ConcreteSyntax . Seq.fromList)
    parsePrecedenceLevel

-- | Parse a precedence level, eg:
--
-- @
--   - Add(x; y) ~ x "*" y
--     Sub(x; y) ~ x "-" y
-- @
parsePrecedenceLevel
  :: ConcreteSyntaxDescriptionParser [OperatorName :-> OperatorDirective]
parsePrecedenceLevel = indentBlock scn $ do
  _ <- symbol "-"
  undefined
