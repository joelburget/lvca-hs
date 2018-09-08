{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Brick
import           Control.Lens
import           Control.Monad.Reader
import           Control.Zipper
import           EasyTest

import           Linguist.Brick
import           Linguist.Proceed
import           Linguist.Types

import qualified Linguist.Languages.Document      as Document
import qualified Linguist.Languages.SimpleExample as SimpleExample
import qualified Linguist.Languages.Stlc          as Stlc
import qualified Linguist.Languages.TExample      as TExample

natJudgement :: JudgementForm
natJudgement = JudgementForm "nat" [(In, "a")]

natJudgements :: JudgementRules
natJudgements = JudgementRules
  [ []
    .--
    ["zero"] %%% "nat"
  , [["a"] %%% "nat"]
    .--
    ["succ" @@ ["a"]] %%% "nat"
  ]

--

main :: IO ()
main = do
  _ <- defaultMain app $
    let env = (SimpleExample.syntax, SimpleExample.dynamics)
        steps :: [StateStep SimpleExample.E]
        steps = iterate (\tm -> runReader (proceed tm) env) $
          StateStep [] (Descending SimpleExample.tm1)
    in zipper steps & fromWithin traverse
  pure ()

allTests :: Test ()
allTests = scope "all tests" $ tests
  [ "toPattern"              toPatternTests
  , "stlc"                   Stlc.stlcTests
  , "matches"                SimpleExample.matchesTests
  , "minus"                  SimpleExample.minusTests
  , "completePatternTests"   SimpleExample.completePatternTests
  , "simple-example"         SimpleExample.dynamicTests
  , "pretty-syntax"          SimpleExample.prettySyntaxChartTests
  , "syntax-statics"         SimpleExample.prettyStaticTests
  , "simple-example.eval"    SimpleExample.evalTests
  , "t-example.eval"         TExample.evalTests
  , "document"               Document.documentTests
  ]

-- main :: IO ()
-- main = do
--   putStrLn "syntax:\n"
--   putDoc (pretty eChart)
--   putStrLn "\n"
--   putStrLn "judgements:\n"
--   putDoc (pretty natJudgements)
--   putStrLn "\n"
