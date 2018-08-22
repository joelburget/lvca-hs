{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Brick
import           Control.Lens
import           Control.Zipper
import           EasyTest

import           Linguist.Brick
import           Linguist.Proceed
import           Linguist.Types

import           Linguist.SimpleExample (denotationTests)
import           Linguist.Stlc
import           Linguist.TExample ()

import Data.Void (Void)

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
    let steps :: [StateStep Void]
        steps = iterate (proceed denotation) $ StateStep [] (Left stlcTm2)
    in zipper steps & fromWithin traverse
  pure ()

allTests :: Test ()
allTests = scope "all tests" $ tests
  [ "toPattern" toPatternTests
  , "matches" matchesTests
  , "minus" minusTests
  , "mkCompletePatternTests" mkCompletePatternTests
  , "simple-example" denotationTests
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
