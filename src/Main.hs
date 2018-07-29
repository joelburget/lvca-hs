{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Brick
import           Control.Lens
import           Control.Zipper
import qualified Data.Map.Strict as Map

import Linguist.Brick
import Linguist.SimpleExample
import Linguist.Types

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
    let steps = iterate proceed $ StateStep (EvalContext [] (Map.empty)) eTerm1
    in zipper steps & fromWithin traverse
  pure ()

-- main :: IO ()
-- main = do
--   putStrLn "syntax:\n"
--   putDoc (pretty eChart)
--   putStrLn "\n"
--   putStrLn "judgements:\n"
--   putDoc (pretty natJudgements)
--   putStrLn "\n"
