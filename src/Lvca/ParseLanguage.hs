module Lvca.ParseLanguage where

import           Data.Text                     (Text)
import           Data.Void                     (Void)
import           Text.Megaparsec

data Language

type LanguageParser a = Parsec
  Void -- error type
  Text -- Stream type
  a

parseLanguage :: LanguageParser Language
parseLanguage = undefined
