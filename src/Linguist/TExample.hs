{-# LANGUAGE OverloadedStrings #-}
module Linguist.TExample where

import qualified Data.Map.Strict as Map

import           Linguist.Types

data T

tChart :: SyntaxChart
tChart = SyntaxChart $ Map.fromList
  [ ("Typ", Sort ["t"]
    [ Operator "nat" (Arity []) "naturals"
    , Operator "arr" (Arity [Valence [] "Typ", Valence [] "Typ"]) "functions"
    ])
  , ("Exp", Sort ["e"]
    [ Operator "z" (Arity []) "zero"
    , Operator "s" (Arity [Valence [] "Exp"]) "successor"
    , Operator "rec" (Arity
      [ Valence [] "Exp"
      , Valence ["Exp", "Exp"] "Exp"
      , Valence [] "Exp"
      ])
      "recursion"
    , Operator "lam" (Arity [Valence [] "Typ", Valence ["Exp"] "Exp"]) "abstraction"
    -- TODO: how to subscript?
    -- TODO: rename Arity
    -- TODO: sugar for these definitions
    , Operator "ap" (Arity [Valence [] "Exp", Valence [] "Exp"]) "application"
    ])
  ]

z, sz, pos, lam, lamapp :: Term T
z = Term "z" []
sz = Term "s" [z]
pos = Term "rec" [sz, z, sz]
-- TODO: how to refer to variable?
lam = Term "lam" [Term "nat" [], z]
lamapp = Term "ap" [lam, sz]

denotation :: DenotationChart T
denotation = DenotationChart
  -- [ (PatternVar "x", Variable "x")
  [ (PatternTm "z" [], Value)
  , (PatternTm "s" [PatternVar "x"], Value) -- TODO: how to say "value if child is value"
  -- , ("rec", )
  -- , ("lam", )
  -- , ("app", )
  ]
