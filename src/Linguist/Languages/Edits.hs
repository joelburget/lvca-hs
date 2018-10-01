{-# LANGUAGE QuasiQuotes #-}
module Linguist.Languages.Edits where

import           Data.Void                       (Void)
import           NeatInterpolation
import           Text.Megaparsec                 (ParseError, runParser)

import           Linguist.ParseSyntaxDescription
import           Linguist.Types                  (SyntaxChart)

syntax :: Either (ParseError Char Void) SyntaxChart
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
