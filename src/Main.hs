{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Brick
import           Control.Lens
import           Control.Zipper
import           EasyTest

import           Linguist.Brick
import           Linguist.Proceed
import           Linguist.Types

import qualified Linguist.SimpleExample as SimpleExample
import           Linguist.Stlc
import           Linguist.TExample ()

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
    let steps :: [StateStep SimpleExample.E]
        steps = iterate (proceed SimpleExample.syntax SimpleExample.dynamics) $
          StateStep [] (Descending SimpleExample.tm1)
    in zipper steps & fromWithin traverse
  pure ()

allTests :: Test ()
allTests = scope "all tests" $ tests
  [ "toPattern" toPatternTests
  , "matches" matchesTests
  , "minus" SimpleExample.minusTests
  , "mkCompletePatternTests" SimpleExample.mkCompletePatternTests
  , "simple-example" SimpleExample.dynamicTests
  , "stlc" stlcTests
  ]

-- main :: IO ()
-- main = do
--   putStrLn "syntax:\n"
--   putDoc (pretty eChart)
--   putStrLn "\n"
--   putStrLn "judgements:\n"
--   putDoc (pretty natJudgements)
--   putStrLn "\n"
