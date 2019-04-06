module Lvca.TokenizeConcrete where

import Data.Char (isSpace)
import Data.Foldable (asum)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import Data.Set (Set)
import qualified Data.Set as Set

import Lvca.ParseUtil (sc)

data ConcreteToken
  = Newline
  | VarToken !Text
  | LiteralToken !Text
  deriving (Eq, Show)

tokenizeConcrete
  :: Set Text
  -> Text
  -> Either (ParseErrorBundle Text Void) [ConcreteToken]
tokenizeConcrete kws = runParser (tokenizeConcrete' kws) "(concrete tokenizer)"

tokenizeConcrete' :: MonadParsec e Text m => Set Text -> m [ConcreteToken]
tokenizeConcrete' kws = some $ asum
  [ Newline <$ string "\n" <* sc
  , LiteralToken <$> string "(" <* sc
  , LiteralToken <$> string ")" <* sc
  , other kws
  ]

other :: MonadParsec e Text m => Set Text -> m ConcreteToken
other kws = (lexeme sc . try) (p >>= check)
  where
    p       = Text.pack <$> some (satisfy (not . isSpace))
    check x = pure $ if x `Set.member` kws
                then LiteralToken x
                else VarToken x
