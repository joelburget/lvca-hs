{-# language QuasiQuotes #-}
module Test.ParseConcreteSyntaxDescription where

import Control.Lens
import           Data.Text                             (Text)
import           EasyTest                              -- (Test, property, (===))
import           Text.Megaparsec
  (errorBundlePretty, runParser)
import           NeatInterpolation
import qualified Data.Map.Strict   as Map

import Lvca.ParseConcreteSyntaxDescription
import Lvca.ParseTerm
import Lvca.Types

abstractSyntax :: SyntaxChart
abstractSyntax = SyntaxChart (Map.fromList
  [ ("ty", SortDef []
    [ Operator "bool" (Arity [])          "booleans"
    , Operator "arr" (Arity ["ty", "ty"]) "arrows"
    ])

  , ("tm", SortDef []
    [ Operator "true"  (Arity [])                          "true"
    , Operator "false" (Arity [])                          "false"
    , Operator "annot" (Arity ["tm", "ty"])                "annotation"
    , Operator "ite"   (Arity ["tm", "tm", "tm"])          "if-then-else"
    , Operator "lam"   (Arity ["ty", Valence ["tm"] "tm"]) "abstraction"
    , Operator "app"   (Arity ["tm", "tm"])                "application"
    ])
  ]) "tm"

testFile :: Text
testFile = [text|
- true()          ~ "true";
  false()         ~ "false";
  bool()          ~ "bool";
- annot(tm; ty)   ~ tm ":" ty;
- ite(t1; t2; t3) ~ "if" t1 "then" t2 "else" t3;
- lam(x. t)       ~ "\" x "." t;
- app(t1; t2)     ~ t1 t2;
- arr(t1; t2)     ~ t1 "->" t2;
  |]

testParseConcreteSyntax :: Test
testParseConcreteSyntax = example $
  case runParser parseConcreteSyntaxDescription "(test)" testFile of
    Left err       -> crash $ errorBundlePretty err
    Right rules'   -> success

  -- where env = ParseEnv abstractSyntax "tm" UntaggedExternals noExternalParsers

doIt = case runParser parseConcreteSyntaxDescription "(test)" testFile of
  Left err -> putStrLn $ errorBundlePretty err
  Right x  -> print x
