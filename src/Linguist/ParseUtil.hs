module Linguist.ParseUtil where

import           Control.Monad              (MonadPlus, void)
import           Data.Text                  (Text, pack)
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


lineComment, blockComment
  :: MonadParsec e Text m => m ()
lineComment  = L.skipLineComment "//"
blockComment = L.skipBlockComment "/*" "*/"

scn :: MonadParsec e Text m => m ()
scn = L.space space1 lineComment blockComment

sc :: MonadParsec e Text m => m ()
sc = L.space (void $ takeWhile1P Nothing f) lineComment blockComment
  where
    f x = x == ' ' || x == '\t'

lexeme :: MonadParsec e Text m => m a -> m a
lexeme = L.lexeme sc

symbol :: MonadParsec e Text m => Text -> m Text
symbol = L.symbol sc

parens :: MonadParsec e Text m => m a -> m a
parens = between (symbol "(") (symbol ")")

brackets :: MonadParsec e Text m => m a -> m a
brackets = between (symbol "[") (symbol "]")

oxfordBrackets :: MonadParsec e Text m => m a -> m a
oxfordBrackets = between (symbol "[[") (symbol "]]")

-- TODO: this can't handle escapes
stringLiteral :: MonadParsec e Text m => m Text
stringLiteral = fmap pack $ char '"' >> manyTill L.charLiteral (char '"')

parseName :: MonadParsec e Text m => m Text
parseName = pack <$> ((:) <$> letterChar <*> many alphaNumChar <* sc)
  <?> "variable"

countSepBy :: MonadPlus m => Int -> m a -> m sep -> m [a]
countSepBy 0 _ _ = pure []
countSepBy n ma sep = do
  a  <- ma
  _  <- sep
  as <- countSepBy (pred n) ma sep
  pure (a:as)

parseVoid :: MonadParsec e t m => m Void
parseVoid = empty <?> "void parse"

noParse :: MonadParsec e t m => m a
noParse = empty <?> "no parse"
