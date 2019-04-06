module Lvca.ParseUtil where

import           Control.Monad              (MonadPlus, liftM, void)
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
lexeme = L.lexeme scn

symbol :: MonadParsec e Text m => Text -> m Text
symbol = L.symbol scn

symbol' :: MonadParsec e Text m => Text -> m Text
symbol' = L.symbol sc

parens :: MonadParsec e Text m => m a -> m a
parens = between (symbol "(") (symbol ")")

brackets :: MonadParsec e Text m => m a -> m a
brackets = between (symbol "[") (symbol "]")

braces :: MonadParsec e Text m => m a -> m a
braces = between (symbol "{") (symbol "}")

oxfordBrackets :: MonadParsec e Text m => m a -> m a
oxfordBrackets = between (symbol "[[") (symbol "]]")

-- https://stackoverflow.com/a/24106749/383958
stringLiteral :: forall e m. MonadParsec e Text m => m Text
stringLiteral = fmap (pack . concat) $ char '"' *> many character <* char '"'
  where

    escapeChars :: String
    escapeChars = "\\\"0nrvtbf"

    nonEscapeChars :: String
    nonEscapeChars = "\\\"\0\n\r\v\t\b\f"

    escape :: m String
    escape = do
      d <- char '\\'
      c <- oneOf escapeChars -- all the characters which can be escaped
      return [d, c]

    nonEscape :: m Char
    nonEscape = noneOf nonEscapeChars

    character :: m String
    character = fmap return nonEscape <|> escape

intLiteral :: (MonadParsec e Text m, Num a, Integral a) => m a
intLiteral = L.signed sc (lexeme L.decimal)

parseName :: MonadParsec e Text m => m Text
parseName = pack
  <$> ((:)
       <$> (letterChar <|> char '_'
         <?> "variable beginning character")
       <*> many (alphaNumChar <|> char '\'' <|> char '_'
         <?> "variable continuing character")
       <*  scn)
  <?> "variable"

-- TODO: isn't this really countEndBy?
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

-- | Like 'endBy' but runs its parser in a 'try'.
endBy' :: (MonadPlus m, MonadParsec e s m) => m a -> m sep -> m [a]
endBy' p sep = many $ try $ do
  x <- p
  re x sep
{-# INLINE endBy' #-}

re :: Monad m => a -> m b -> m a
re x = liftM (const x)
{-# INLINE re #-}
