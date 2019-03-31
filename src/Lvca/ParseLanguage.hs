module Lvca.ParseLanguage where

import           Data.Text                     (Text)
import           Data.Void                     (Void)
import           Text.Megaparsec

import qualified Lvca.Bidirectional as Bidir
import Lvca.Types (SyntaxChart, ConcreteSyntax, Sort)
import Lvca.ParseSyntaxDescription
import Lvca.ParseConcreteSyntaxDescription
import Lvca.ParseBidirectional
import Lvca.ParseTerm

type LanguageParser a = Parsec
  Void -- error type
  Text -- Stream type
  a

data Lang = Lang
  !SyntaxChart
  !ConcreteSyntax
  ![Bidir.Rule]

parseLang :: Sort -> LanguageParser Lang
parseLang sort = do
  abstractSyntax <- parseSyntaxDescription
  concreteSyntax <- parseConcreteSyntaxDescription $
    ParseEnv abstractSyntax sort UntaggedExternals noExternalParsers
  statics        <- parseBidirectional
  pure $ Lang abstractSyntax concreteSyntax statics
