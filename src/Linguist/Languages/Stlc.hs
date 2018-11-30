{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
module Linguist.Languages.Stlc (stlcTests) where

import qualified Data.Map.Strict                 as Map
import           Data.Void                       (Void)
import           EasyTest
import           NeatInterpolation
import           Text.Megaparsec                 (errorBundlePretty, runParser)

import           Linguist.ParseLanguage
import           Linguist.ParseSyntaxDescription hiding (Parser)
import           Linguist.Types                  hiding (Term)
import qualified Linguist.Types                  as L


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

-- data TermF e
--   = LamF   !e
--   | ApF !e !e

-- data Term
--   = Lam !(Either Term Pat)
--   | Ap  !(Either Term Pat) !(Either Term Pat)

-- dynamics :: DenotationChart' (TermF :+: PatF) MeaningF
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

stlcTests :: Test ()
stlcTests = tests
  [ scope "match" $
      expectJust $ runMatches stlcChart "Exp" $ matches
        (PatternTm "lam"
          [ PatternVar (Just "ty")
          , BindingPattern ["x"] (PatternVar (Just "body"))
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
           L.Term "lam"
             [ L.Term "nat" []
             , L.Binding [ "a" ] (L.Var "a")
             ]
         ]
  ]

stlcTm1, stlcTm2 :: L.Term Void
stlcTm1 = L.Term "lam" [Var "ty", Binding ["x"] (L.Term "ap" [Var "x", Var "x"])]
stlcTm2 = L.Term "ap" [stlcTm1, stlcTm1]
