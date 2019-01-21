{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
module Languages.Stlc where

import qualified Data.Map.Strict                 as Map
import           Data.Void                       (Void)
import           EasyTest
import           NeatInterpolation
import           Text.Megaparsec                 (errorBundlePretty, runParser)

import Lvca
import Lvca.Types (matches)

import Test.Types
import Test.ParseTerm

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


stlcTm1, stlcTm2 :: Term Void
stlcTm1 = Fix $ Term "lam"
  [ Fix $ Var "ty"
  , Fix $ Binding ["x"] $ Fix $ Term "ap"
    [ Fix $ Var "x"
    , Fix $ Var "x"
    ]
  ]
stlcTm2 = Fix $ Term "ap" [stlcTm1, stlcTm1]

stlcTests :: Test ()
stlcTests = tests
  [ scope "match" $
      expectJust $ runMatches stlcChart "Exp" $ matches
        (PatternTm "lam"
          [ PatternVar (Just "ty")
          , PatternVar (Just "body")
          ])
        stlcTm1
  , scope "parsing chart" $
    let result = runParser parseSyntaxDescription "(test)"
          [text|
            Typ ::= nat               "natural numbers"
                    arr(Typ; Typ)     "arrows"
            Exp ::= lam(Typ; Exp.Exp) "abstraction"
                    ap(Exp; Exp)      "application"
          |]
    in case result of
         Left err     -> crash $ errorBundlePretty err
         Right parsed -> expectEq parsed stlcChart
  , scope "different indentation" $
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
         Left err     -> crash $ errorBundlePretty err
         Right parsed -> expectEq parsed stlcChart

  , scope "prop_parse_pretty" $ testProperty $ prop_parse_pretty stlcChart "Exp"
    (const Nothing) noExternalParsers

   , scope "prop_serialise_identity" $ testProperty $
     prop_serialise_identity @() stlcChart "Exp" (const Nothing)

  , let expectParse = parseTest $
          ParseEnv stlcChart "Exp" UntaggedExternals noExternalParsers

    in scope "parse" $ tests
         [ expectParse "lam(ty; x. ap(x; x))" stlcTm1
         , expectParse [text| ap(
               lam(ty; x. ap(x; x));
               lam(ty; x. ap(x; x))
             )
             |]
             stlcTm2
         , expectParse "lam(nat(); a. a)" $
           Fix $ Term "lam"
             [ Fix $ Term "nat" []
             , Fix $ Binding [ "a" ] $ Fix $ Var "a"
             ]
         ]
  ]
