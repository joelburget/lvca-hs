{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE TypeFamilies #-}
module Languages.Stlc where

import           Control.Lens      (_Just)
import qualified Data.Map.Strict   as Map
import           Data.Void         (Void)
import           EasyTest          hiding (matches)
import qualified EasyTest          as E
import           NeatInterpolation
import           Text.Megaparsec   (errorBundlePretty, runParser)

import           Lvca
import           Lvca.Types        (matches)

import           Test.ParseTerm
import           Test.Types

stlcChart :: SyntaxChart
stlcChart = SyntaxChart (Map.fromList
  [ ("Typ", SortDef []
    [ Operator "nat" (FixedArity [])
    , Operator "arr" (FixedArity ["Typ", "Typ"])
    ])
  , ("Exp", SortDef []
    [ Operator "lam" (FixedArity ["Typ", FixedValence ["Exp"] "Exp"])
    , Operator "ap"  (FixedArity ["Exp", "Exp"])

    --
    -- , Operator "s"   (FixedArity ["Exp"])
    -- , Operator "z"   (FixedArity [])
    ])
  ])

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
stlcTm1 = Term "lam"
  [ Scope [] $ Var "ty"
  , Scope ["x"] $ Term "ap"
    [ Scope [] $ Var "x"
    , Scope [] $ Var "x"
    ]
  ]
stlcTm2 = Term "ap" [Scope [] stlcTm1, Scope [] stlcTm1]

stlcTests :: Test
stlcTests = tests
  [ scope "match" $ example $
      E.matches _Just $ runMatches stlcChart "Exp" $ matches
        (PatternTm "lam"
          [ PatternVar (Just "ty")
          , PatternVar (Just "body")
          ])
        stlcTm1
  , scope "parsing chart" $ example $
    let result = runParser parseSyntaxDescription' "(test)"
          [text|
            Exp ::= lam(Typ; Exp.Exp)
                    ap(Exp; Exp)
            Typ ::= nat
                    arr(Typ; Typ)
          |]
    in case result of
         Left err     -> crash $ errorBundlePretty err
         Right parsed -> parsed === stlcChart
  , scope "different indentation" $ example $
    let result = runParser parseSyntaxDescription' "(test)"
          [text|
            /* more comments */
            Exp ::=
              lam(Typ; Exp.Exp)
              ap(Exp; Exp)

            Typ ::= // testing comments
              nat
              arr(Typ; Typ)
          |]
    in case result of
         Left err     -> crash $ errorBundlePretty err
         Right parsed -> parsed === stlcChart

  , scope "prop_parse_abstract_pretty" $
    prop_parse_abstract_pretty stlcChart "Exp"
    (const Nothing) noExternalParsers

   , scope "prop_serialise_identity" $
     prop_serialise_identity @() stlcChart "Exp" (const Nothing)

  , let expectParse = standardParseTermTest $
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
           Term "lam"
             [ Scope [] $ Term "nat" []
             , Scope [ "a" ] $ Var "a"
             ]
         ]
  ]
