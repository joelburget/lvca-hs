{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE KindSignatures #-}
module Linguist.Languages.TExample where

import Control.Lens (Prism', prism', review, preview)
import qualified Data.Map.Strict  as Map
import           EasyTest
import           Prelude          hiding (succ)
import Data.Text (Text)

import           Linguist.Proceed (eval)
import           Linguist.Types
import           Linguist.Languages.MachineModel
import Linguist.FunctorUtil


data T

instance Show T where show = \case
instance Eq   T where (==) = \case

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

z, sz, ssz, pos, succ, lamapp, lamapp2 :: Term T
z = Term "z" []
sz = Term "s" [z]
ssz = Term "s" [sz]
pos = Term "rec" [sz, z, sz]
succ = Term "lam" [Term "nat" [], Binding ["x"] (Term "s" [Var "x"])]
lamapp = Term "ap" [succ, z]
lamapp2 = Term "ap" [succ, lamapp]

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
    FreeIn (Value (FreeIn (Sv (Pure "e"))))
  -- -- TODO: rec
  , FixIn (Ap
      (FixIn (Lam (FixIn (BindingPatF ["arg'"] (FixIn (PatVarF (Just "body")))))))
      (FixIn (PatVarF (Just "arg"))))
    :->
    FreeIn (Eval (Pure "arg") "arg'" (Pure "body"))
  ]

dynamics :: DenotationChart T Text
dynamics = mkDenotationChart id p1 p2 dynamics' where

  p1' :: Prism' (Pattern T) (Fix (PatF :+: TermF))
  p1' = patTermP p1

  p1 :: Prism' (Pattern T) (TermF (Fix (PatF :+: TermF)))
  p1 = prism' rtl ltr where
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

  p2' :: Prism' (Term Text) (Free (MeaningF :+: ValF) Text)
  p2' = meaningTermP id p2

  p2 :: Prism' (Term Text) (ValF (Free (MeaningF :+: ValF) Text))
  p2 = prism' rtl ltr where
    rtl :: ValF (Free (MeaningF :+: ValF) Text) -> Term Text
    rtl = \case
      Zv   -> Term "Zv" []
      Sv v -> Term "Sv" [review p2' v]
    ltr = \case
      Term "Zv" []  -> Just Zv
      Term "Sv" [t] -> Sv <$> preview p2' t
      _             -> Nothing

evalTests :: Test ()
evalTests =
  let eval' = eval "Exp" syntax dynamics id (error "no primitives")
  in tests
       [ expect $ eval' z       == Right z
       , expect $ eval' sz      == Right sz
       , expect $ eval' lamapp  == Right sz
       , expect $ eval' lamapp2 == Right ssz
       ]
