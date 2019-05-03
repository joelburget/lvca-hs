{-# language QuasiQuotes #-}
module Test.ParseConcreteSyntaxDescription where

import Control.Monad.IO.Class

import Control.Lens
import           Data.Text                             (Text)
import qualified Data.Text.IO                          as Text
import           EasyTest
import           Text.Megaparsec
  (eof, errorBundlePretty, runParser)
import           NeatInterpolation
import qualified Data.Map.Strict   as Map
import Hedgehog (PropertyT)

import Lvca.ParseConcreteSyntaxDescription
import Lvca.ParseTerm
import Lvca.ParseUtil (scn)
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
TERMINAL_IDENT    := [-_A-Z][A-Z0-9]*
NONTERMINAL_IDENT := [-_a-z][a-z0-9]*
METACHAR          :=  [\\\|\*\+\.\[\]\(\)]
REGULAR_CHAR      := [^\\\|\*\+\.\[\]\(\)]

ty :=
  | bool()              ~ "bool"
  | arr(t1: ty; t2: ty) ~ t1 "->" t2

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

parseTest' :: ConcreteSyntaxDescriptionParser a -> Text -> PropertyT IO ()
parseTest' p input = case runParser (p <* scn <* eof) "test" input of
  Left err       -> crash $ errorBundlePretty err
  Right rules'   -> success

parseTest :: ConcreteSyntaxDescriptionParser a -> Text -> Test
parseTest p input = example $ parseTest' p input

parseTests :: Test
parseTests = scope "parse-concrete-syntax-description" $ tests
  [ parseTest syntaxDescription testFile
  , parseTest stringLiteral     [text|"true"|]
  , parseTest sort              "(foo bar) (baz quux)"
  , parseTest namedSort         "tm: ty[n]"
  , parseTest abstractValence   "x: tm. t: tm"
  , parseTest abstractPat       "lam(x: tm. t: tm)"
  , parseTest terminalRule      "TERMINAL_IDENT := [A-Z][-_A-Z0-9]*"
  , parseTest regex             "."
  , parseTest regex             "[A-Z]"
  , parseTest regex             "[A-Z][-_A-Z0-9]*"
  , parseTest regex             [text|[\\\|\*\+\.\[\(\)]|]
  , parseTest regex             [text|[^\\\|\*\+\.\[\(\)]|]
  , parseTest nonterminalRule [text|
nonterminal-ctor :=
  | nonterminal-ctor[n](abstract-pat; matches: nonterminal-match[n])
  ~ abstract-pat ("~" match[i]){i:n}
    |]

  ]
