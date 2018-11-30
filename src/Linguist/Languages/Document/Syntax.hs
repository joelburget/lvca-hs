{-# LANGUAGE QuasiQuotes #-}

module Linguist.Languages.Document.Syntax where

import           Data.Text         (Text)
import           NeatInterpolation

syntaxText :: Text
syntaxText =
  [text|
// TODO: would be nice to have some sort of built-in sequences
Document ::= Document(List Block)

Block ::=
  Header(HeaderLevel; {Text})
  Paragraph(Inline)
  BlockEmbed{BlockEmbed}

HeaderLevel ::=
  H1
  H2
  H3

Inline ::= Inline(List InlineAtom)

InlineAtom ::=
  // ideally a list of attributes but sets are much harder to model
  InlineAtom(Maybe Attribute; {Text})
  {InlineEmbed}

Attribute ::=
  Bold
  Italic

List a ::=
  Nil
  Cons(a; List a)

Maybe a ::=
  Nothing
  Just(a)
  |]
