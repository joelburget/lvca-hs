{-# language QuasiQuotes #-}
module Test.ParseConcreteSyntaxDescription where

import Control.Monad.IO.Class

import Control.Lens
import           Data.Text                             (Text)
import           EasyTest                              -- (Test, property, (===))
import           Text.Megaparsec
  (eof, errorBundlePretty, runParser)
import           NeatInterpolation
import qualified Data.Map.Strict   as Map

import Lvca.ParseConcreteSyntaxDescription
import Lvca.ParseTerm
import Lvca.Types

abstractSyntax :: SyntaxChart
abstractSyntax = SyntaxChart (Map.fromList
  [ ("ty", SortDef []
    [ Operator "bool" (FixedArity [])
    , Operator "arr" (FixedArity ["ty", "ty"])
    ])

  , ("tm", SortDef []
    [ Operator "true"  (FixedArity [])
    , Operator "false" (FixedArity [])
    , Operator "annot" (FixedArity ["tm", "ty"])
    , Operator "ite"   (FixedArity ["tm", "tm", "tm"])
    , Operator "lam"   (FixedArity ["ty", FixedValence ["tm"] "tm"])
    , Operator "app"   (FixedArity ["tm", "tm"])
    ])
  ])

testFile :: Text
testFile = [text|
ty :=
  | bool()              // ~ "bool"
  | arr(t1: ty; t2: ty) // ~ t1 "->" t2

tm :=
  | true()
  ~ "true"
  | false()
  ~ "false"
  | annot(tm; ty)
  ~ tm ":" ty
  | ite(t1: tm; t2: tm; t3: tm)
  ~ "if" t1 "then" t2 "else" t3
  | lam(x: tm. t: tm)
  ~ "\\" x "." t
  | app(tm; tm)
  ~ assocl
  |]

testParseConcreteSyntax :: Test
testParseConcreteSyntax = example $
  case runParser (syntaxDescription <* eof) "test" testFile of
    Left err       -> crash $ errorBundlePretty err
    Right rules'   -> success

testParseMatch :: Test
testParseMatch = example $
  case runParser stringLiteral "test" [text|"true"|] of
    Left err       -> crash $ errorBundlePretty err
    Right rules'   -> success
