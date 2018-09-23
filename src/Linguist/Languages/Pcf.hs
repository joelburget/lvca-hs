{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE PatternSynonyms   #-}
module Linguist.Languages.Pcf where

import           Data.Text                          (Text)
import           Data.Void                          (Void)
import           NeatInterpolation

import           Linguist.Types

-- PCF allows general recursion. It's important that a semantics is forced to
-- acknowledge this.

syntaxText :: Text
syntaxText =
  [text|
Typ ::=
  Nat
  Parr(Typ; Typ)

Exp ::=
  Z
  S(Exp)
  Ifz(Exp; Exp. Exp)
  Lam(Typ; Exp. Exp)
  Ap (Exp;      Exp)
  Fix(Typ; Exp. Exp)
  |]

-- TODO: statics

(|->) :: Pattern -> Denotation a -> (Pattern, Denotation a)
(|->) = (,)

pattern Ifz :: Pattern
pattern Ifz = (PatternTm "Ifz"
      [ PatternVar (Just "e0")
      , BindingPattern ["x"] (PatternVar (Just "e1"))
      ])

pattern Ap :: Pattern -> Pattern -> Pattern
pattern a `Ap` b = PatternTm "Ap" [a, b]

pattern FixPat =
  PatternTm "Fix" [ PatternAny, BindingPattern ["x"] (PatternVar (Just "e")) ]

denotation :: DenotationChart Void
denotation = DenotationChart
  [ PatternTm "S" [ PatternVar (Just "e") ] |->
    EvalThen ("e" |-> "e'") (PatternTm "S" [ PatternVar (Just "e'") ])
  , Ifz `Ap` PatternTm "Z" []
    |-> Choose "e0"
  , Ifz `Ap` PatternTm "S" [PatternVar (Just "e")]
    |-> Cbv "e1" ["e"]
  , PatternTm "Lam" [ PatternAny, BindingPattern ["x"] (PatternVar (Just "e")) ]
    `Ap`
    PatternVar (Just "e2")
    |->
    Cbv "e" ["e2"]

  -- Implement self-reference by substituting the recursive expression itself
  -- for the variable "x" in the body
  , FixPat |-> VarSubst "x" "e" FixPat
  ]
