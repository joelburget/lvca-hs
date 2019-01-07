module Main where

import           Brick
import           Brick.BChan
import qualified Brick.Widgets.Border as B
import           Control.Concurrent (forkFinally)
-- import           Control.Lens
import           Control.Exception (SomeException)
import           Control.Monad (void)
import           Control.Monad.IO.Class
-- import           Control.Monad.Reader
-- import           Control.Zipper
-- import           Data.Void (Void)
import qualified Graphics.Vty as V
import           System.Console.Haskeline
import qualified System.Console.Haskeline.Brick as HB
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

data Event = FromHBWidget HB.ToBrick
           | HaskelineDied (Either SomeException ())

runHaskeline :: HB.Config Event -> IO ()
runHaskeline c = runInputTBehavior (HB.useBrick c) defaultSettings mainLoop

mainLoop :: InputT IO ()
mainLoop = do
  -- TODO: use color library
  -- minput <- getInputLine "\ESC[35mlvca> \ESC[m "
  minput <- getInputLine "> "
  case minput of
    Nothing  -> return ()
    Just ""  -> mainLoop
    Just cmdline -> case words cmdline of
      [ "edit", _ident ] -> do
        void $ liftIO $ do
          (_, _, _, p) <- createProcess (proc "nvim" [])
          waitForProcess p
        mainLoop
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

data Name = HaskelineWidget
    deriving (Ord, Eq, Show)

data Cmd

newtype AppState = AppState [ Cmd ]
  -- { haskelineWidget :: HB.Widget Name }

initialState :: AppState
initialState = AppState []
  -- { haskelineWidget = HB.initialWidget HaskelineWidget }

app :: HB.Config Event -> App AppState Event Name
app c = App { appDraw = drawUI
            , appChooseCursor = \_ -> showCursorNamed HaskelineWidget
            , appHandleEvent = handleEvent c
            , appStartEvent = return
            , appAttrMap = const $ attrMap V.defAttr []
            }

handleEvent :: HB.Config Event
            -> AppState -> BrickEvent Name Event -> EventM Name (Next AppState)
handleEvent c s@AppState{haskelineWidget = hw} e = do
    hw' <- HB.handleEvent c hw e
    handleAppEvent (s { haskelineWidget = hw' }) e

handleAppEvent :: AppState -> BrickEvent Name Event -> EventM Name (Next AppState)
handleAppEvent s (AppEvent (HaskelineDied _)) = halt s
handleAppEvent s (VtyEvent (V.EvKey V.KEsc [])) = halt s
handleAppEvent s _ = continue s

drawUI :: AppState -> [Widget Name]
drawUI s = [ B.border $ HB.render "foo" ] -- $ haskelineWidget s ]

main :: IO ()
main = runInputT defaultSettings $ liftIO $ do
  chan <- newBChan 10
  config <- HB.configure
          chan
          FromHBWidget
          (\case { FromHBWidget x -> Just x; _ -> Nothing })
  _ <- forkFinally
          (runHaskeline config)
          (writeBChan chan . HaskelineDied)

  void $ customMain
      (V.mkVty V.defaultConfig)
      (Just chan)
      (app config)
      initialState

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
