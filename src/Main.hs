module Main where

-- import           Brick
-- import           Control.Lens
import           Control.Monad.Reader
-- import           Control.Zipper
-- import           Data.Text                 (Text)
-- import qualified Data.Text                 as Text
-- import           Data.Void                 (Void, absurd)
-- import           Data.Text.Prettyprint.Doc

-- import           Lvca.Brick
-- import           Lvca.Proceed (proceed, translate)
import           Lvca.Types
-- import           Lvca.Util (forceRight)

import qualified Languages.Arith      as Arith
-- import           Lvca.Languages.MachineModel

-- import           Control.Monad.Reader               (runReaderT, runReader)
-- import           Control.Monad.Trans.Maybe
-- import           Control.Monad.Writer.CPS

natJudgement :: JudgementForm
natJudgement = JudgementForm "nat" [(JIn, "a")]

natJudgements :: JudgementRules
natJudgements = JudgementRules
  [ []
    :--
    ["zero"] :%%% "nat"
  , [["a"] :%%% "nat"]
    :--
    ["succ" :@@ ["a"]] :%%% "nat"
  ]

--

-- instance Pretty (Either Text Void) where
--   pretty = \case
--     Left t  -> pretty $ Text.unpack t
--     Right v -> absurd v

main :: IO ()
main = do

--   putStrLn "term:"
--   print Arith.example
--   putStrLn "\ndynamics"
--   print $ pretty $ forceRight Arith.peanoDynamics
--   putStrLn "\nunmade chart"
--   print $ unMkDenotationChart Arith.patP Arith.termP2 (forceRight Arith.peanoDynamics)
--   let Just chart =
--         unMkDenotationChart Arith.patP Arith.termP2 (forceRight Arith.peanoDynamics)
--   print $ preview Arith.termP1 Arith.example
--   let Just tm' = preview Arith.termP1 Arith.example
--   let (result, logs) = runWriter $ runMaybeT $ translate @_ @Arith.Arith chart tm'
--   putStrLn "\nresult:"
--   print result
--   putStrLn "\nlogs:"
--   traverse (putStrLn . Text.unpack) logs

  case Arith.peanoProceed Arith.example of
      Left err    -> putStrLn "1" >> putStrLn err
      Right steps -> putStrLn "2" >> (void $ traverse print steps)
      -- void $ defaultMain app $
      --   trace ("steps: " ++ show steps) $
      --   State (zipper steps & fromWithin traverse) False

-- main :: IO ()
-- main = do
--   putStrLn "syntax:\n"
--   putDoc (pretty eChart)
--   putStrLn "\n"
--   putStrLn "judgements:\n"
--   putDoc (pretty natJudgements)
--   putStrLn "\n"
