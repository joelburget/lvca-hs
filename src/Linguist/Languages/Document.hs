{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Linguist.Languages.Document where

import           Control.Lens
import           Data.Text                       (Text)
import           Data.Void                       (Void)
import           NeatInterpolation
import           Text.Megaparsec                 (ParseError, runParser)

import           Linguist.ParseSyntaxDescription
import           Linguist.Types                  (SyntaxChart, Term(..))

import Data.Diverse

syntax :: Either (ParseError Char Void) SyntaxChart
syntax = runParser parseSyntaxDescription "(document syntax)"
  [text|
// TODO: would be nice to have some sort of built-in sequences
Document ::= Document(List(Block))

Block ::=
  Header(HeaderLevel; Text)
  Paragraph(Inline)
  BlockEmbed[BlockEmbed]

HeaderLevel ::=
  H1
  H2
  H3

Inline ::= Inline(List(InlineAtom))

InlineAtom ::=
  // ideally a list of attributes but sets are much harder to model
  InlineAtom(List(Attribute); [Text])
  InlineEmbed[InlineEmbed]

Attribute ::=
  Bold
  Italic
  Underline

List a ::=
  Nil
  Cons(a; List(a))
  |]

data InlineEmbed

newtype Document blockEmbed inlineEmbed = Document [Block blockEmbed inlineEmbed]

data Block blockEmbed inlineEmbed
  = Header !HeaderLevel !Text
  | Paragraph !(Inline inlineEmbed)
  | BlockEmbed !blockEmbed

data HeaderLevel = H1 | H2 | H3

newtype Inline inlineEmbed = Inline [InlineAtom inlineEmbed]

data InlineAtom inlineEmbed
  = InlineAtom ![Attribute] !Text
  | InlineEmbed !inlineEmbed

data Attribute = Bold | Italic | Underline

type Term' a b = Term (Which '[a, b, Text])

model :: Prism' (Term' a b) (Document a b)
model = prism' forward backward where

  forward :: Document a b -> Term' a b
  forward (Document blocks) = Term "Document"
    [review (listP blockPrism) blocks]

  backward :: Term' a b -> Maybe (Document a b)
  backward = \case
    Term "Document" [blockList]
      -> Document <$> preview (listP blockPrism) blockList
    _ -> Nothing

listP :: Prism' (Term b) a -> Prism' (Term b) [a]
listP p = prism' (forward (review p)) (backward (preview p)) where

  forward :: (a -> Term b) -> [a] -> Term b
  forward f = \case
    []   -> Term "nil" []
    x:xs -> Term "cons" [f x, forward f xs]

  backward :: (Term b -> Maybe a) -> Term b -> Maybe [a]
  backward f = \case
    Term "nil" []       -> Just []
    Term "cons" [x, xs] -> (:) <$> f x <*> backward f xs
    _                   -> Nothing

blockPrism :: Prism' (Term' a b) (Block a b)
blockPrism = prism' forward backward where

  backward :: forall a b. Term' a b -> Maybe (Block a b)
  backward = \case
    Term "Header" [level, PrimValue val] -> Header
      <$> preview headerLevelPrism level
      <*> trialN' @2 val
    Term "Paragraph" [inline] -> Paragraph <$> preview inlineP inline
    Term "BlockEmbed" [PrimValue val] -> BlockEmbed <$> trialN' @0 val
    _ -> Nothing

  forward :: Block a b -> Term' a b
  forward = \case
    Header level t -> Term "Header"
      [ (review headerLevelPrism) level
      , PrimValue (pickN @2 t)
      ]
    Paragraph inline -> Term "Paragraph" [review inlineP inline]
    BlockEmbed embed -> Term "BlockEmbed" [PrimValue (pickN @0 embed)]

headerLevelPrism :: Prism' (Term x) HeaderLevel
headerLevelPrism = prism' forward backward where

  forward h =
    let h' = case h of
          H1 -> "H1"
          H2 -> "H2"
          H3 -> "H3"
    in Term h' []

  backward = \case
    Term h [] -> case h of
      "H1" -> Just H1
      "H2" -> Just H2
      "H3" -> Just H3
      _    -> Nothing
    _ -> Nothing

inlineP :: Prism' (Term' a b) (Inline b)
inlineP = prism' forward backward where

  forward :: Inline b -> Term' a b
  forward (Inline inlines)
    = Term "Inline" [review (listP inlineAtomP) inlines]

  backward = \case
    Term "Inline" [atoms] -> Inline <$> preview (listP inlineAtomP) atoms
    _                     -> Nothing

inlineAtomP :: Prism' (Term' a b) (InlineAtom b)
inlineAtomP = prism' forward backward where

  forward :: InlineAtom b -> Term' a b
  forward = \case
    InlineAtom attrs t -> Term "InlineAtom"
      [ review (listP attributeP) attrs
      , PrimValue (pickN @2 t)
      ]
    InlineEmbed embed -> Term "InlineEmbed" [PrimValue (pickN @1 embed)]

  backward = \case
    Term "InlineAtom" [attrs, PrimValue val] -> InlineAtom
      <$> preview (listP attributeP) attrs
      <*> trialN' @2 val
    Term "InlineEmbed" [PrimValue val] -> InlineEmbed <$> trialN' @1 val
    _ -> Nothing

attributeP :: Prism' (Term x) Attribute
attributeP = prism' forward backward where

  forward attr =
    let attr' = case attr of
          Bold      -> "Bold"
          Italic    -> "Italic"
          Underline -> "Underline"
    in Term attr' []

  backward = \case
    Term attr [] -> case attr of
      "Bold"      -> Just Bold
      "Italic"    -> Just Italic
      "Underline" -> Just Underline
      _           -> Nothing
    _ -> Nothing
