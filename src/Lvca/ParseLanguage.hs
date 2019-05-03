module Lvca.ParseLanguage where

import           Data.Text                     (Text)
import           Data.Void                     (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import qualified Lvca.Bidirectional as Bidir
import Lvca.DenotationChart
import qualified Lvca.Types as Core
import Lvca.ParseBidirectional
import qualified Lvca.ParseConcreteSyntaxDescription as SD
import Lvca.ParseDenotationChart
-- import Lvca.ParseSyntaxDescription
import Lvca.ParseUtil (symbol, symbol', parseName, sc, scn)

type LanguageParser a = Parsec
  Void -- error type
  Text -- Stream type
  a

data Lang = Lang
  { _languageName   :: !Text
  , _abstractSyntax :: !Core.SyntaxChart
  , _concreteSyntax :: !Core.ConcreteSyntax
  , _statics        :: ![Bidir.Rule]
  , _dynamics       :: !DenotationChart
  }

translateSyntaxDesc
  :: SD.ConcreteSyntax
  -> Either String (Core.SyntaxChart, Core.ConcreteSyntax)
translateSyntaxDesc = undefined

parseHeader :: MonadParsec e Text m => m b -> m b
parseHeader parseBody = symbol' "=" *> parseBody <* sc <* symbol "="

parseSubheader :: MonadParsec e Text m => m b -> m b
parseSubheader parseBody = symbol' "==" *> parseBody <* sc <* symbol "=="

parseLang :: LanguageParser Lang
parseLang = do
  name <- parseHeader $ string "language " >> parseName

  _ <- parseSubheader $ string "abstract syntax"
  syntaxDesc <- SD.syntaxDescription
  _ <- scn

  (abstractSyntax, concreteSyntax) <- case translateSyntaxDesc syntaxDesc of
    Left msg     -> error msg
    Right (a, c) -> pure (a, c)

  _       <- parseSubheader $ string "statics"
  statics <- parseBidirectional

  _        <- parseSubheader $ string "dynamics"
  dynamics <- parseDenotationChart

  pure $ Lang name abstractSyntax concreteSyntax statics dynamics
