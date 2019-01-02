module Main where

import           Brick
import           Control.Lens
import           Control.Monad.Reader
import           Control.Zipper
import           Data.Void (Void)

import           Lvca.Brick
import           Lvca.Proceed ()
import           Lvca.Types
import           Lvca.Util (forceRight)

import qualified Lvca.Languages.Arith         as Arith
import           Lvca.Languages.MachineModel


natJudgement :: JudgementForm
natJudgement = JudgementForm "nat" [(JIn, "a")]

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
    -- let env = (SimpleExample.syntax, SimpleExample.dynamics)
    --     steps :: [StateStep SimpleExample.E]
    --     steps = iterate (\tm -> runReader (proceed tm) env) $
    --       StateStep [] (Descending SimpleExample.tm1)
    let env = (Arith.syntax, forceRight Arith.machineDynamics, const Nothing)
        steps :: [StateStep Void]
        steps = iterate (\tm -> runReader (error "TODO" tm) env) $
          StateStep [] (Descending Arith.example)
    in State (zipper steps & fromWithin traverse) False
  pure ()

-- main :: IO ()
-- main = do
--   putStrLn "syntax:\n"
--   putDoc (pretty eChart)
--   putStrLn "\n"
--   putStrLn "judgements:\n"
--   putDoc (pretty natJudgements)
--   putStrLn "\n"
