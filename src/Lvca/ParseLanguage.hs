module Lvca.ParseLanguage where

import           Data.Text                     (Text)
import           Data.Void                     (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

import qualified Lvca.Bidirectional as Bidir
import Lvca.DenotationChart
import Lvca.Types (SyntaxChart) -- , ConcreteSyntax)
import Lvca.ParseBidirectional
import Lvca.ParseConcreteSyntaxDescription
import Lvca.ParseDenotationChart
import Lvca.ParseSyntaxDescription
import Lvca.ParseUtil (symbol, symbol', parseName, sc, scn)

type LanguageParser a = Parsec
  Void -- error type
  Text -- Stream type
  a

data Lang = Lang
  { _languageName   :: !Text
  , _abstractSyntax :: !SyntaxChart
  , _concreteSyntax :: !ConcreteSyntax
  , _statics        :: ![Bidir.Rule]
  , _dynamics       :: !DenotationChart
  }

parseHeader :: MonadParsec e Text m => m b -> m b
parseHeader parseBody = symbol' "=" *> parseBody <* sc <* symbol "="

parseSubheader :: MonadParsec e Text m => m b -> m b
parseSubheader parseBody = symbol' "==" *> parseBody <* sc <* symbol "=="

parseLang :: LanguageParser Lang
parseLang = do
  name <- parseHeader $ string "language " >> parseName

  _ <- parseSubheader $ string "abstract syntax"
  abstractSyntax <- parseSyntaxDescription
  _ <- scn

  _ <- parseSubheader $ string "concrete syntax"
  undefined

--   concreteSyntax <- parseConcreteSyntaxDescription
--     -- TODO: we must check that the top-level sort has kind *
--     -- ParseEnv abstractSyntax (SortAp sort []) UntaggedExternals noExternalParsers

--   _ <- parseSubheader $ string "statics"
--   statics        <- parseBidirectional

--   _ <- parseSubheader $ string "dynamics"
--   dynamics       <- parseDenotationChart

--   pure $ Lang name abstractSyntax concreteSyntax statics dynamics
