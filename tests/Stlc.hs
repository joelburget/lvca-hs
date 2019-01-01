module Test.Stlc where

import           NeatInterpolation
import           Text.Megaparsec                 (errorBundlePretty, runParser)

import           Linguist.ParseLanguage
import           Linguist.ParseSyntaxDescription hiding (Parser)

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
           Fix $ L.Term "lam"
             [ Fix $ L.Term "nat" []
             , Fix $ L.Binding [ "a" ] $ Fix $ L.Var "a"
             ]
         ]
  ]
