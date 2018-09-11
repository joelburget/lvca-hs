{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
module Linguist.Languages.Document where

import qualified CMark as MD
import qualified CMark.Patterns as MD
import           Control.Lens hiding (from, to)
import           Data.Diverse
import           Data.Text                       (Text, pack)
import           Data.Void                       (absurd, Void)
import           EasyTest
import           GHC.Generics
import           NeatInterpolation
import           Text.Megaparsec                 (ParseError, runParser)

import           Linguist.ParseSyntaxDescription
import           Linguist.Types                  (SyntaxChart, Term(..))

syntax :: Either (ParseError Char Void) SyntaxChart
syntax = runParser parseSyntaxDescription "(document syntax)"
  [text|
// TODO: would be nice to have some sort of built-in sequences
Document ::= Document(List Block)

Block ::=
  Header(HeaderLevel; Text)
  Paragraph(Inline)
  BlockEmbed[BlockEmbed]

HeaderLevel ::=
  H1
  H2
  H3

Inline ::= Inline(List InlineAtom)

InlineAtom ::=
  // ideally a list of attributes but sets are much harder to model
  InlineAtom(Maybe Attribute; [Text])
  InlineEmbed[InlineEmbed]

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

data InlineEmbed
  deriving Generic

newtype Document blockEmbed inlineEmbed = Document [Block blockEmbed inlineEmbed]
  deriving Generic

data Block blockEmbed inlineEmbed
  = Header !HeaderLevel !Text
  | Paragraph !(Inline inlineEmbed)
  | BlockEmbed !blockEmbed
  deriving Generic

data HeaderLevel = H1 | H2 | H3
  deriving Generic

newtype Inline inlineEmbed = Inline [InlineAtom inlineEmbed]
  deriving Generic

data InlineAtom inlineEmbed
  = InlineAtom !(Maybe Attribute) !Text
  | InlineEmbed !inlineEmbed
  deriving Generic

data Attribute = Bold | Italic
  deriving Generic

--

constrName :: (HasConstructor (Rep a), Generic a) => a -> String
constrName = genericConstrName . from

class HasConstructor (f :: * -> *) where
  genericConstrName :: f x -> String

instance HasConstructor f => HasConstructor (D1 c f) where
  genericConstrName (M1 x) = genericConstrName x

instance (HasConstructor x, HasConstructor y) => HasConstructor (x :+: y) where
  genericConstrName (L1 l) = genericConstrName l
  genericConstrName (R1 r) = genericConstrName r

instance Constructor c => HasConstructor (C1 c f) where
  genericConstrName x = conName x

--

instance TermPrism (Block a b) (Which '[a, b, Text])

helperIso :: Generic a => Iso' (Rep a x) a
helperIso = iso to from

class TermPrism tm a where
  termPrism :: Prism' (Term a) tm
  default termPrism :: (Generic tm, GPrism (Rep tm) a) => Prism' (Term a) tm
  termPrism = gPrism . helperIso

class TermLens tm a where
  termLens :: Lens' (Term a) tm
  default termLens :: (Generic tm, GLens (Rep tm) a) => Lens' (Term a) tm
  termLens = gLens . helperIso

class GPrism f a where
  gPrism :: Prism' (Term a) (f x)

class GLens f a where
  gLens :: Lens' (Term a) (f x)

instance GPrism V1 Void where
  gPrism = prism' undefined (const Nothing)

instance GPrism f a => GPrism (M1 i c f) a where
  gPrism = gPrism . m1Iso
-- instance GPrism (K1 i c) a where
instance GPrism (f :+: g) a where
  gPrism = prism'
    (\case
      L1 x -> Term "TODO" _
      R1 x -> _)
    _

-- instance GPrism (f :*: g) a where

-- instance GPrism U1 () where
--   gPrism = prism'
--     (\_u -> Term (pack (constrName undefined)) [])
--     _

m1Iso :: Iso' (M1 i c f p) (f p)
m1Iso = iso unM1 M1

instance GLens f a => GLens (M1 i c f) a where
  gLens = gLens . m1Iso
instance GLens (K1 i c) a where
instance GLens (f :*: g) a where

instance GLens U1 () where
  -- TODO: this can't be right
  gLens = lens (const U1) const

class GSum f where
  conName' :: f x -> Text

instance (GSum f, GSum g) => GSum (f :+: g) where
  conName' = \case
    L1 f -> conName' f
    R1 f -> conName' f

instance (Constructor c, i ~ C, GSum f) => GSum (M1 i c f) where
  conName' = pack . conName

instance (GSum f, GSum g, GPrism f a, GPrism g b)
  => GPrism (f :+: g) (Either a b) where
  gPrism = prism'
    (\case
      L1 x -> Term (conName' x) [Left  <$> review gPrism x]
      R1 y -> Term (conName' y) [Right <$> review gPrism y])
    (\(Term name subterms) ->
      if | name == conName' (L1 undefined :: (f :+: g) a)
         -> undefined -- L1 . undefined <$> (traverse (preview gPrism) subterms)
         | name == conName' (R1 undefined :: (f :+: g) a)
         -> undefined -- R1 . undefined <$> traverse (preview gPrism) subterms
         | otherwise
         -> Nothing
    )

type Term' a b = Term (Which '[a, b, Text])

model :: Prism' (Term' a b) (Document a b)
model = prism' bwd fwd where

  bwd = \case
    Document blocks -> Term "Document" [review (listP blockP) blocks]

  fwd = \case
    Term "Document" [blockList]
      -> Document <$> preview (listP blockP) blockList
    _ -> Nothing

maybeP :: Prism' (Term b) a -> Prism' (Term b) (Maybe a)
maybeP p = prism' bwd fwd where

  bwd = \case
    Nothing -> Term "Nothing" []
    Just x  -> Term "Just" [review p x]

  fwd = \case
    Term "Nothing" [] -> Just Nothing
    Term "Just" [x]   -> Just <$> preview p x
    _                 -> Nothing

listP :: Prism' (Term b) a -> Prism' (Term b) [a]
listP p = prism' bwd fwd where

  bwd = \case
    []   -> Term "Nil" []
    x:xs -> Term "Cons" [review p x, bwd xs]

  fwd = \case
    Term "Nil" []       -> Just []
    Term "Cons" [x, xs] -> (:) <$> preview p x <*> fwd xs
    _                   -> Nothing

blockEmbedPrism :: Prism' (Which '[a, b, Text]) a
blockEmbedPrism = prism' (pickN @0) (trialN' @0)

inlineEmbedPrism :: Prism' (Which '[a, b, Text]) b
inlineEmbedPrism = prism' (pickN @1) (trialN' @1)

textPrism :: Prism' (Which '[a, b, Text]) Text
textPrism = prism' (pickN @2) (trialN' @2)

blockP :: forall a b. Prism' (Term' a b) (Block a b)
blockP = termPrism -- prism' bwd fwd where

--   fwd = \case
--     Term "Header" [level, PrimValue val] -> Header
--       <$> preview headerLevelP level
--       <*> preview textPrism val
--     Term "Paragraph" [inline] -> Paragraph <$> preview inlineP inline
--     Term "BlockEmbed" [PrimValue val] ->
--       BlockEmbed <$> preview blockEmbedPrism val
--     _ -> Nothing

--   bwd = \case
--     Header level t -> Term "Header"
--       [ (review headerLevelP) level
--       , PrimValue (review textPrism t)
--       ]
--     Paragraph inline -> Term "Paragraph" [review inlineP inline]
--     BlockEmbed embed
--       -> Term "BlockEmbed" [PrimValue (review blockEmbedPrism embed)]

headerLevelP :: Prism' (Term x) HeaderLevel
headerLevelP = prism' bwd fwd where

  bwd h =
    let h' = case h of
          H1 -> "H1"
          H2 -> "H2"
          H3 -> "H3"
    in Term h' []

  fwd = \case
    Term h [] -> case h of
      "H1" -> Just H1
      "H2" -> Just H2
      "H3" -> Just H3
      _    -> Nothing
    _ -> Nothing

inlineP :: Prism' (Term' a b) (Inline b)
inlineP = prism' bwd fwd where

  bwd (Inline inlines)
    = Term "Inline" [review (listP inlineAtomP) inlines]

  fwd = \case
    Term "Inline" [atoms] -> Inline <$> preview (listP inlineAtomP) atoms
    _                     -> Nothing

inlineAtomP :: Prism' (Term' a b) (InlineAtom b)
inlineAtomP = prism' bwd fwd where

  bwd :: InlineAtom b -> Term' a b
  bwd = \case
    InlineAtom attrs t -> Term "InlineAtom"
      [ review (maybeP attributeP) attrs
      , PrimValue (review textPrism t)
      ]
    InlineEmbed embed
      -> Term "InlineEmbed" [PrimValue (review inlineEmbedPrism embed)]

  fwd = \case
    Term "InlineAtom" [attrs, PrimValue val] -> InlineAtom
      <$> preview (maybeP attributeP) attrs
      <*> preview textPrism val
    Term "InlineEmbed" [PrimValue val]
      -> InlineEmbed <$> preview inlineEmbedPrism val
    _ -> Nothing

attributeP :: Prism' (Term x) Attribute
attributeP = prism' bwd fwd where

  bwd attr =
    let attr' = case attr of
          Bold      -> "Bold"
          Italic    -> "Italic"
    in Term attr' []

  fwd = \case
    Term attr [] -> case attr of
      "Bold"      -> Just Bold
      "Italic"    -> Just Italic
      _           -> Nothing
    _ -> Nothing

inlineAtomMdP :: forall b. Prism' MD.Node b -> Prism' MD.Node (InlineAtom b)
inlineAtomMdP embedP = prism' bwd fwd where

  bwd = \case
    InlineAtom Nothing t       -> MD.Text_ t
    InlineAtom (Just Bold) t   -> MD.Strong_ [MD.Text_ t]
    InlineAtom (Just Italic) t -> MD.Emph_ [MD.Text_ t]
    InlineEmbed b              -> review embedP b

  fwd = \case
   MD.Text_ t              -> Just $ InlineAtom Nothing t
   MD.Strong_ [MD.Text_ t] -> Just $ InlineAtom (Just Bold) t
   MD.Emph_ [MD.Text_ t]   -> Just $ InlineAtom (Just Italic) t
   _                       -> Nothing

blockMdP
  :: forall inlineEmbed blockEmbed.
     Prism' MD.Node blockEmbed
  -> Prism' MD.Node inlineEmbed
  -> Prism' MD.Node (Block blockEmbed inlineEmbed)
blockMdP blockEmbedP inlineEmbedP = prism' bwd fwd where

  bwd = \case
    Header h t ->
      let h' = case h of
            H1 -> 1
            H2 -> 2
            H3 -> 3
      in MD.Heading_ h' [MD.Text_ t]
    Paragraph (Inline inlines) -> MD.Paragraph_ $
      fmap (review (inlineAtomMdP inlineEmbedP)) inlines
    BlockEmbed embed -> review blockEmbedP embed

  fwd = \case
    MD.Heading_ h [MD.Text_ t] ->
      let h' = case h of
            1 -> Just H1
            2 -> Just H2
            3 -> Just H3
            _ -> Nothing
      in Header <$> h' <*> pure t
    MD.Paragraph_ inlines -> Paragraph . Inline
      <$> traverse (preview (inlineAtomMdP inlineEmbedP)) inlines
    node -> BlockEmbed <$> preview blockEmbedP node

documentMdP
  :: forall inlineEmbed blockEmbed.
     Prism' MD.Node blockEmbed
  -> Prism' MD.Node inlineEmbed
  -> Prism' MD.Node (Document blockEmbed inlineEmbed)
documentMdP blockEmbedP inlineEmbedP = prism' bwd fwd where
  blockMdP' :: Prism' MD.Node (Block blockEmbed inlineEmbed)
  blockMdP' = blockMdP blockEmbedP inlineEmbedP
  bwd = \case
    Document blocks -> MD.Document_ $ fmap (review blockMdP') blocks
  fwd = \case
    MD.Document_ blocks -> Document <$> traverse (preview blockMdP') blocks
    _                   -> Nothing

blockEmbedVoidP :: Prism' MD.Node Void
blockEmbedVoidP = prism' absurd (const Nothing)

inlineEmbedVoidP :: Prism' MD.Node Void
inlineEmbedVoidP = prism' absurd (const Nothing)

documentMdP' :: Prism' MD.Node (Document Void Void)
documentMdP' = documentMdP blockEmbedVoidP inlineEmbedVoidP

textMdP :: Iso' Text MD.Node
textMdP
  = iso (MD.commonmarkToNode options) (MD.nodeToCommonmark options Nothing)
  where options = []

foldText :: Fold Text (Term' Void Void)
foldText = textMdP . documentMdP' . re model

foldTerm :: Fold (Term' Void Void) Text
foldTerm = model . re documentMdP' . re textMdP

textDocument :: Text
textDocument = [text|
# important document

*this* is the important document you've heard about. **never** mention it to anyone
|]

documentTests :: Test ()
documentTests = tests
  [ do Just doc  <- pure $ textDocument ^? foldText
       Just doc' <- pure $ doc ^? foldTerm
       expectEq textDocument doc'
  ]
