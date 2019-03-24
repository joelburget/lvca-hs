module Lvca.ParseLanguage where

type LanguageParser a = Parsec
  Void -- error type
  Text -- Stream type
  a

parseLanguage :: LanguageParser Language
parseLanguage = undefined
