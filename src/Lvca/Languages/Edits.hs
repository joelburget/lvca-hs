{-# LANGUAGE QuasiQuotes #-}
module Lvca.Languages.Edits where

import           Data.Text                       (Text)
import           Data.Void                       (Void)
import           NeatInterpolation
import           Text.Megaparsec                 (ParseErrorBundle, runParser)

import           Lvca.ParseSyntaxDescription
import           Lvca.Types                  (SyntaxChart)

syntax :: Either (ParseErrorBundle Text Void) SyntaxChart
syntax = runParser parseSyntaxDescription "(edits syntax)"
  [text|
pattern ::=
  PatternTm([text]; list(pattern))
  BindingPattern(list([text]); pattern) "a pattern binding a list of names
  PatternVar(maybe(text))               "a variable with an optional name"
  PatternPrimVal(text; maybe(text))     "a prim val is an external name, eg 'str', and an optional variable name"
  PatternUnion(list(pattern))

list a ::=
  EmptyList
  Cons(a; list a)

maybe a ::=
  Nothing
  Just(a)
  |]
