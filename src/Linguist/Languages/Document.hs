{-# LANGUAGE QuasiQuotes #-}
module Linguist.Languages.Document where

import           Data.Void                       (Void)
import           NeatInterpolation
import           Text.Megaparsec                 (ParseError, runParser)

import           Linguist.ParseSyntaxDescription
import           Linguist.Types                  (SyntaxChart)

syntax :: Either (ParseError Char Void) SyntaxChart
syntax = runParser parseSyntaxDescription "(document syntax)"
  [text|
// TODO: would be nice to have some sort of built-in sequences
document ::=
  empty
  blocks(block; document)

block ::=
  header(headerlevel; text)
  paragraph(inline)
  blockembed[blockembed]

headerlevel ::=
  h1
  h2
  h3

inline ::=
  // TODO: can operators for different sorts have the same name?
  empty
  // TODO: maybe {utf8bytes} is a better syntax for externals
  // Some text with attributes, followed by the rest of the text
  inlineseq(attributes; [utf8bytes]; inline)
  inlineembed[inlineembed]

attributes ::=
  bold
  italic
  underline
  |]
