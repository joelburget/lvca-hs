{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies       #-}
module Linguist.Languages.Stlc where

import qualified Data.Map.Strict                 as Map
import           Data.Text                       (pack)
import           Data.Void                       (Void)
import           EasyTest
import           NeatInterpolation
import           Text.Megaparsec                 (errorBundlePretty, runParser)

import           Linguist.FunctorUtil
import           Linguist.ParseSyntaxDescription
import           Linguist.Types hiding (Term)
import qualified Linguist.Types as L
import           Linguist.Languages.MachineModel


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

data TermF e
  = LamF   !e
  | ApF !e !e

data Term
  = Lam !(Either Term Pat)
  | Ap  !(Either Term Pat) !(Either Term Pat)

type instance Base Term = TermF :+: PatF

-- mkDenotationChart' :: [(a, b)] -> DenotationChart' (Base a) (Base b)
-- mkDenotationChart' = undefined

dynamics :: DenotationChart' (TermF :+: PatF) MeaningF
dynamics = DenotationChart'
  [ FixL (ApF
      (FixL (LamF (FixR (BindingPatF ["x"] (FixIn (PatVarF (Just "body")))))))
      (FixR (PatVarF (Just "applicand"))))
    :->
    Free
      (Eval (Free (MeaningVar "applicand"))
      "foo"
      (Free (MeaningVar "body")))
  ]

stlcTests :: Test ()
stlcTests = tests
  -- [ expectJust $ runMatches stlcChart "Exp" $ findMatch denotation
  --   stlcTm2
  [ expectJust $ runMatches stlcChart "Exp" $ matches
    (PatternTm "lam" [BindingPattern ["x"] (PatternVar (Just "body"))])
    stlcTm1
  , "parsing chart" $
    let result = runParser parseSyntaxDescription "(test)"
          [text|
            Typ ::= nat               "natural numbers"
                    arr(Typ; Typ)     "arrows"
            Exp ::= lam(Typ; Exp.Exp) "abstraction"
                    ap(Exp; Exp)      "application"
          |]
    in case result of
         Left err     -> crash $ pack $ errorBundlePretty err
         Right parsed -> expectEq parsed stlcChart
  , "different indentation" $
    let result = runParser parseSyntaxDescription "(test)"
          [text|
            Typ ::= // testing comments
              nat               "natural numbers"
              arr(Typ; Typ)     "arrows"

            /* more comments */
            Exp ::=
              lam(Typ; Exp.Exp) "abstraction"
              ap(Exp; Exp)      "application"
          |]
    in case result of
         Left err     -> crash $ pack $ errorBundlePretty err
         Right parsed -> expectEq parsed stlcChart
  ]

stlcTm1, stlcTm2 :: L.Term Void
stlcTm1 = L.Term "lam" [Binding ["x"] (L.Term "ap" [Var "x", Var "x"])]
stlcTm2 = L.Term "ap" [stlcTm1, stlcTm1]
