{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Linguist.Languages.TExample where

import qualified Data.Map.Strict  as Map
import           EasyTest
import           Prelude          hiding (succ)

import           Linguist.Proceed (eval)
import           Linguist.Types


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

dynamics :: DenotationChart T
dynamics = DenotationChart
  -- [ (PatternVar "x", Variable "x")
  [ (PatternTm "z" [], Value)
  -- TODO: how to say "value if child is value"?
  , (PatternTm "s" [PatternVar (Just "x")], Value)
  -- , ("rec", )
  , (PatternTm "ap"
    [PatternTm "lam"
      -- TODO:
      -- # which of these is more expressive?
      -- # change stlc if we go the uncommented route
      -- [PatternAny, BindingPattern ["x"] (PatternVar (Just "body"))]
      [PatternAny, PatternVar (Just "body")]
    , PatternVar (Just "applicand")
    ], Cbv "body" ["applicand"])
  ]

evalTests :: Test ()
evalTests = tests
  [ expect $ eval "Exp" syntax dynamics z       == Right z
  , expect $ eval "Exp" syntax dynamics sz      == Right sz
  , expect $ eval "Exp" syntax dynamics lamapp  == Right sz
  , expect $ eval "Exp" syntax dynamics lamapp2 == Right ssz
  ]
