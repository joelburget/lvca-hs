import           EasyTest

import qualified Linguist.Languages.Arith         as Arith
import qualified Linguist.Languages.Document      as Document
import           Linguist.Languages.MachineModel
import qualified Linguist.Languages.SimpleExample as SimpleExample
import qualified Linguist.Languages.Stlc          as Stlc
import qualified Linguist.Languages.TExample      as TExample

main :: IO ()
main = run allTests

allTests :: Test ()
allTests = scope "all tests" $ tests
  [ scope "toPattern"              toPatternTests
  , scope "stlc"                   Stlc.stlcTests
  , scope "matches"                SimpleExample.matchesTests
  , scope "minus"                  SimpleExample.minusTests
  , scope "completePatternTests"   SimpleExample.completePatternTests
  , scope "simple-example"         SimpleExample.dynamicTests
  , scope "pretty-syntax"          SimpleExample.prettySyntaxChartTests
  , scope "syntax-statics"         SimpleExample.prettyStaticTests
  , scope "simple-example.eval"    SimpleExample.evalTests
  , scope "simple-example.parse"   SimpleExample.parseTests
  , scope "simple-example.props"   SimpleExample.propTests
  -- , scope "t-example.eval"         TExample.evalTests
  , scope "document"               Document.documentTests
  , scope "arith"                  Arith.arithTests
  ]
