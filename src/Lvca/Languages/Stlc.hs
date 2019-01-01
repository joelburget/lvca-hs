{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
module Lvca.Languages.Stlc  where

import qualified Data.Map.Strict                 as Map
import           Data.Void                       (Void)

import           Lvca.FunctorUtil
import           Lvca.Types                  hiding (Term)
import qualified Lvca.Types                  as L


stlcChart :: SyntaxChart
stlcChart = SyntaxChart $ Map.fromList
  [ ("Typ", SortDef []
    [ Operator "nat" (Arity []) "natural numbers"
    , Operator "arr" (Arity ["Typ", "Typ"]) "arrows"
    ])
  , ("Exp", SortDef []
    [ Operator "lam" (Arity ["Typ", Valence ["Exp"] "Exp"]) "abstraction"
    , Operator "ap"  (Arity ["Exp", "Exp"])                 "application"

    --
    -- , Operator "s"   (Arity ["Exp"])                        "successor"
    -- , Operator "z"   (Arity [])                             "zero"
    ])
  ]

-- denotation :: DenotationChart Void Text
-- denotation = mkDenotationChart
--   [ (PatternTm "ap"
--     [ PatternTm "lam" [BindingPattern ["x"] (PatternVar (Just "body"))]
--     , PatternVar (Just "applicand")
--     ],
--     Cbn "body" "applicand")
--   ]

-- data ExpF e
--   = LamF   !e
--   | ApF !e !e

-- data Term
--   = Lam !(Either Term Pat)
--   | Ap  !(Either Term Pat) !(Either Term Pat)

-- dynamics :: DenotationChart' (ExpF :+: PatF) MeaningF
-- dynamics = DenotationChart'
--   [ FixL (ApF
--       (FixL (LamF (FixR (BindingPatF ["x"] (FixIn (PatVarF (Just "body")))))))
--       (FixR (PatVarF (Just "applicand"))))
--     :->
--     Free
--       (Eval (Pure "applicand")
--       "foo"
--       (Pure "body"))
--   ]


stlcTm1, stlcTm2 :: L.Term Void
stlcTm1 = Fix $ L.Term "lam"
  [ Fix $ Var "ty"
  , Fix $ Binding ["x"] $ Fix $ L.Term "ap"
    [ Fix $ Var "x"
    , Fix $ Var "x"
    ]
  ]
stlcTm2 = Fix $ L.Term "ap" [stlcTm1, stlcTm1]
