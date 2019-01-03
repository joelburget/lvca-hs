module Main where

-- import           Brick
-- import           Control.Lens
import           Control.Monad (void)
import           Control.Monad.IO.Class
-- import           Control.Monad.Reader
-- import           Control.Zipper
-- import           Data.Void (Void)
import           System.Console.Haskeline
import           System.Process

-- import           Lvca.Brick
import           Lvca.Proceed ()
import           Lvca.Types
-- import           Lvca.Util (forceRight)

-- import qualified Lvca.Languages.Arith         as Arith
-- import           Lvca.Languages.MachineModel


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
main = runInputT defaultSettings mainLoop

mainLoop :: InputT IO ()
mainLoop = do
  -- TODO: use color library
  minput <- getInputLine "\ESC[35mlvca> \ESC[m" -- "> "
  case minput of
    Nothing  -> return ()
    Just ""  -> mainLoop
    Just cmdline -> case words cmdline of
      [ "edit", _ident ] -> void $ liftIO $ do
        (_, _, _, p) <- createProcess (proc "nvim" [])
        waitForProcess p
      [ "modify", _ref, _f ] -> do
        outputStrLn "\ESC[35mTODO\ESC[m"
        mainLoop
      [ "set", _ref, _id ] -> do
        outputStrLn "\ESC[35mTODO\ESC[m"
        mainLoop
      [ "eval", _ref ] -> do
        outputStrLn "\ESC[35mTODO\ESC[m"
        mainLoop
      _ -> do
        outputStrLn "\ESC[35munrecognized command\ESC[m"
        mainLoop

-- main :: IO ()
-- main = do
--   _ <- defaultMain app $
--     -- let env = (SimpleExample.syntax, SimpleExample.dynamics)
--     --     steps :: [StateStep SimpleExample.E]
--     --     steps = iterate (\tm -> runReader (proceed tm) env) $
--     --       StateStep [] (Descending SimpleExample.tm1)
--     let env = (Arith.syntax, forceRight Arith.machineDynamics, const Nothing)
--         steps :: [StateStep Void]
--         steps = iterate (\tm -> runReader (error "TODO" tm) env) $
--           StateStep [] (Descending Arith.example)
--     in State (zipper steps & fromWithin traverse) False
--   pure ()

-- main :: IO ()
-- main = do
--   putStrLn "syntax:\n"
--   putDoc (pretty eChart)
--   putStrLn "\n"
--   putStrLn "judgements:\n"
--   putDoc (pretty natJudgements)
--   putStrLn "\n"
