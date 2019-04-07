module Lvca.TokenizeConcrete (Token(..), tokenizeConcrete) where

import Data.Char (isPunctuation, isSymbol)
import Data.Foldable (asum)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import           Text.Megaparsec hiding (Token)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Set (Set)
import qualified Data.Set as Set

import Lvca.ParseUtil (sc, stringLiteral)

data Token
  = Newline
  | Var !Text
  | Keyword !Text
  | StringLiteral !Text
  | NatLiteral !Integer
  | Symbols !Text
  deriving (Eq, Show)

tokenizeConcrete
  :: Set Text
  -> Text
  -> Either (ParseErrorBundle Text Void) [Token]
tokenizeConcrete kws = runParser (tokenizeConcrete' kws) "(concrete tokenizer)"

tokenizeConcrete' :: MonadParsec e Text m => Set Text -> m [Token]
tokenizeConcrete' kws = some $ asum
  [ Newline <$ eol <* sc
  , NatLiteral <$> L.lexeme sc L.decimal <* sc
  , StringLiteral <$> stringLiteral <* sc

  , do str <- name
       pure $ if str `Set.member` kws
         then Keyword str
         else Var str

  , do str <- symbols
       pure $ if str `Set.member` kws
         then Keyword str
         else Symbols str
  ]

name :: MonadParsec e Text m => m Text
name = fmap Text.pack ((:) <$> letterChar <*> many alphaNumChar)
  <* sc

symbols :: MonadParsec e Text m => m Text
symbols = fmap Text.pack $
  some (satisfy $ \x -> isPunctuation x || isSymbol x)
  <* sc
