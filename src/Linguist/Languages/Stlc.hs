{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
module Linguist.Languages.Stlc where

import qualified Data.Map.Strict                 as Map
import           Data.Text                       (pack)
import           Data.Void                       (Void)
import           EasyTest
import           NeatInterpolation
import           Text.Megaparsec                 (parseErrorPretty, runParser)

import           Linguist.ParseSyntaxDescription
import           Linguist.Types

stlcChart :: SyntaxChart
stlcChart = SyntaxChart $ Map.fromList
  [ ("Typ", Sort []
    [ Operator "nat" (Arity []) "natural numbers"
    , Operator "arr" (Arity ["Typ", "Typ"]) "arrows"
    ])
  , ("Exp", Sort []
    [ Operator "lam" (Arity ["Typ", Valence ["Exp"] "Exp"]) "abstraction"
    , Operator "ap"  (Arity ["Exp", "Exp"])                 "application"

    --
    -- , Operator "s"   (Arity ["Exp"])                        "successor"
    -- , Operator "z"   (Arity [])                             "zero"
    ])
  ]

denotation :: DenotationChart Void
denotation = DenotationChart
  [ (PatternTm "ap"
    [ PatternTm "lam" [BindingPattern ["x"] (PatternVar (Just "body"))]
    , PatternVar (Just "applicand")
    ],
    Cbn "body" ["applicand"])
  ]

stlcTests :: Test ()
stlcTests = tests
  [ expectJust $ runMatches stlcChart "Exp" $ findMatch denotation
    stlcTm2
  , expectJust $ runMatches stlcChart "Exp" $ matches
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
         Left err     -> crash $ pack $ parseErrorPretty err
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
         Left err     -> crash $ pack $ parseErrorPretty err
         Right parsed -> expectEq parsed stlcChart
  ]

stlcTm1, stlcTm2 :: Term Void
stlcTm1 = Term "lam" [Binding ["x"] (Term "ap" [Var "x", Var "x"])]
stlcTm2 = Term "ap" [stlcTm1, stlcTm1]
