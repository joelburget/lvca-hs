{-# LANGUAGE EmptyCase       #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE KindSignatures  #-}
module Linguist.Languages.TExample where
-- Godel's system t

import Control.Lens (Prism, Prism', prism, prism', review, preview)
import qualified Data.Map.Strict  as Map
import           EasyTest
import           Prelude          hiding (succ)
import Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Prettyprint.Doc (Pretty(pretty))

import           Linguist.Proceed
import           Linguist.Types
import           Linguist.Languages.MachineModel
import Linguist.FunctorUtil

import Debug.Trace

data T

instance Show T where show = absurd
instance Eq   T where (==) = absurd

instance Pretty T where
  pretty = absurd

absurd :: T -> a
absurd = \case

_T :: Prism s s a T
_T = prism absurd Left
{-# INLINE _T #-}

syntax :: SyntaxChart
syntax = SyntaxChart $ Map.fromList
  [ ("Typ", SortDef []
    [ Operator "nat" [] "naturals"
    , Operator "arr" [Valence [] "Typ", Valence [] "Typ"] "functions"
    ])
  , ("Exp", SortDef []
    [ Operator "z" [] "zero"
    , Operator "s" [Valence [] "Exp"] "successor"
    , Operator "rec"
      [ Valence [] "Exp"
      , Valence ["Exp", "Exp"] "Exp"
      , Valence [] "Exp"
      ]
      "recursion"
    , Operator "lam" [Valence [] "Typ", Valence ["Exp"] "Exp"] "abstraction"
    -- TODO: how to subscript?
    -- TODO: sugar for these definitions
    , Operator "ap" [Valence [] "Exp", Valence [] "Exp"] "application"
    ])
  ]

-- TODO: add type annotations
data TermF term
  = Z
  | S               !term
  | Rec !term !term !term
  | Lam             !term
  | Ap        !term !term

data ValF val
  = Zv
  | Sv !val

dynamics' :: DenotationChart' (PatF :+: TermF) (MeaningF :+: ValF)
dynamics' = DenotationChart'
  [ FixIn Z :-> FreeIn (Value (FreeIn Zv))
  , FixIn (S (FixIn (PatVarF (Just "e"))))
    :->
    FreeIn (Eval (Pure "e") "e'" (FreeIn (Value (FreeIn (Sv (Pure "e'"))))))
  -- -- TODO: rec
  , FixIn (Ap
      (FixIn (Lam (FixIn (BindingPatF ["arg'"] (FixIn (PatVarF (Just "body")))))))
      (FixIn (PatVarF (Just "arg"))))
    :->
    FreeIn (Eval (Pure "arg") "arg'" (Pure "body"))
  ]

dynamics :: DenotationChart T Text
dynamics = mkDenotationChart id tmPatP valTmP dynamics'

(//) :: Text -> Text -> Term Text -> Term Text
(//) from to term = Term "Renaming" [PrimValue from, PrimValue to, term]

meaningPatternVar :: a -> Term a
meaningPatternVar name = Term "MeaningPatternVar" [ PrimValue name ]

dynamics2 :: DenotationChart T Text
dynamics2 = DenotationChart
  [ PatternTm "Z" []
    :->
    Term "Value" [Term "Zv" []]
  , PatternTm "S" [PatternVar (Just "e")]
    :->
    Term "Eval"
      [ meaningPatternVar "e"
      , Binding ["e'"] (Term "Value" [Term "Sv" [Var "e'"]])
      ]
  , PatternTm "Ap"
    [ PatternTm "Lam" [PatternAny, BindingPattern ["x"] (PatternVar (Just "body"))]
    , PatternVar (Just "arg")
    ]
    :->
    Term "Eval"
      [ meaningPatternVar "arg"
      , Binding ["arg'"] $
        (Term "Eval"
          [ ("x" // "arg'") (meaningPatternVar "body")
          , Binding ["body'"] (Var "body'")
          ])
      ]
  ]

tmPatP :: Prism' (Pattern T) (TermF (Fix (PatF :+: TermF)))
tmPatP = prism' rtl ltr where
  p1' :: Prism' (Pattern T) (Fix (PatF :+: TermF))
  p1' = patTermP tmPatP

  rtl = \case
    Z -> PatternTm "Z" []
    S a -> PatternTm "S" [review p1' a]
    Rec a b c -> PatternTm "Rec"
      [ review p1' a
      , review p1' b
      , review p1' c
      ]
    Lam a -> PatternTm "Lam" [review p1' a]
    Ap a b -> PatternTm "Ap" [review p1' a, review p1' b]
  ltr = \case
    PatternTm "Z" []          -> Just Z
    PatternTm "S" [a]         -> S <$> preview p1' a
    PatternTm "Rec" [a, b, c] -> Rec
      <$> preview p1' a
      <*> preview p1' b
      <*> preview p1' c
    -- XXX Lam should have binding structure
    PatternTm "Lam" [a]   -> Lam <$> preview p1' a
    PatternTm "Ap" [a, b] -> Ap <$> preview p1' a <*> preview p1' b
    _                     -> Nothing

-- tmTermP :: Prism' (Term T) (TermF (Fix (PatF :+: TermF)))
-- tmTermP = prism' rtl ltr where
--   p1' :: Prism' (Term T) (Fix (PatF :+: TermF))
--   p1' = patTermP tmTermP

--   rtl = \case
--     Z -> PatternTm "Z" []
--     S a -> PatternTm "S" [review p1' a]
--     Rec a b c -> PatternTm "Rec"
--       [ review p1' a
--       , review p1' b
--       , review p1' c
--       ]
--     Lam a -> PatternTm "Lam" [review p1' a]
--     Ap a b -> PatternTm "Ap" [review p1' a, review p1' b]
--   ltr = \case
--     PatternTm "Z" []          -> Just Z
--     PatternTm "S" [a]         -> S <$> preview p1' a
--     PatternTm "Rec" [a, b, c] -> Rec
--       <$> preview p1' a
--       <*> preview p1' b
--       <*> preview p1' c
--     -- XXX Lam should have binding structure
--     PatternTm "Lam" [a]   -> Lam <$> preview p1' a
--     PatternTm "Ap" [a, b] -> Ap <$> preview p1' a <*> preview p1' b
--     _                     -> Nothing

valTmP :: Prism' (Term Text) (ValF (Free (MeaningF :+: ValF) Text))
valTmP = prism' rtl ltr where
  p2' :: Prism' (Term Text) (Free (MeaningF :+: ValF) Text)
  p2' = meaningTermP id valTmP

  rtl :: ValF (Free (MeaningF :+: ValF) Text) -> Term Text
  rtl = \case
    Zv   -> Term "Zv" []
    Sv v -> Term "Sv" [review p2' v]
  ltr = \case
    Term "Zv" []  -> Just Zv
    Term "Sv" [t] -> Sv <$> preview p2' t
    _             -> Nothing

z, sz, ssz, pos, succ, lamapp, lamapp2 :: Term T
z = Term "Z" []
sz = Term "S" [z]
ssz = Term "S" [sz]
pos = Term "Rec" [sz, z, sz]
succ = Term "Lam" [Term "Nat" [], Binding ["x"] (Term "S" [Var "x"])]
lamapp = Term "Ap" [succ, z]
lamapp2 = Term "Ap" [succ, lamapp]

zv, szv, sszv :: Term Text
zv = Term "Zv" []
szv = Term "Sv" [zv]
sszv = Term "Sv" [szv]

eval' :: Term T -> Either String (Term Text)
eval' = eval $
  mkEvalEnv "Exp" syntax dynamics2
    (\name -> trace (Text.unpack name) Nothing) -- (const Nothing)
    (const Nothing)

evalTests :: Test ()
evalTests = tests
  [ expect $ eval' z       == Right zv
  , expect $ eval' sz      == Right szv
  , expect $ eval' lamapp  == Right szv
  , expect $ eval' lamapp2 == Right sszv
  ]
