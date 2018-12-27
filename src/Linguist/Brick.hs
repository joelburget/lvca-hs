{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Linguist.Brick where

import           Brick                      as B
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import           Brick.Widgets.Center       (center)
import           Control.Lens
import           Control.Zipper
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                 (Void, absurd)
import qualified Graphics.Vty               as V
import           NeatInterpolation

import           Linguist.FunctorUtil
import           Linguist.Types
import           Linguist.Languages.MachineModel


class TmShow a where
  drawPrim :: a -> Widget ()

instance (TmShow a, TmShow b) => TmShow (Either a b) where
  drawPrim = either drawPrim drawPrim

instance TmShow Void where
  drawPrim = str . absurd

instance TmShow Int where
  drawPrim = str . show

instance TmShow Text where
  drawPrim = str . show

type ListZipper a = Top :>> [a] :>> a

data State a = State
  { _timeline   :: ListZipper (StateStep a)
  , _showHelp :: Bool
  }

makeLenses ''State

next :: State a -> State a
next state@(State tl _help) = case tl ^. focus of
  Done{} -> state
  _      -> case rightward tl of
    Just tl' -> state & timeline .~ tl'
    Nothing  -> state

prev :: State a -> State a
prev state@(State tl _help) = case leftward tl of
  Just tl' -> state & timeline .~ tl'
  Nothing  -> state

toggleHelp :: State a -> State a
toggleHelp = showHelp %~ not

bordered :: Text -> Widget n -> Widget n
bordered name widget = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (txt name)
  $ padAll 4 widget

redAttr, blueAttr, emptyAttr :: AttrName
redAttr = "redAttr"
blueAttr = "blueAttr"
emptyAttr = "emptyAttr"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (redAttr, fg V.red)
  , (blueAttr, fg V.blue)
  ]

handleEvent
  :: State s
  -> BrickEvent () a
  -> EventM () (Next (State s))
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ prev g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ next g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ next g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ prev g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ prev g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ next g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ next g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ prev g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar '?') [])) = continue $ toggleHelp g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

drawUI :: TmShow s => State s -> Widget ()
drawUI (State tl showHelp') =

  let mainView = case tl ^. focus of
        StateStep ctx valTm ->
          let lBox = bordered "Context" $ center $ drawCtx ctx
              rBox = bordered "Term"    $ center $ drawFocus valTm
          in lBox <+> rBox
        Errored info -> withAttr redAttr $ center $ txt "error " <+> txt info
        Done val     -> bordered "Done"  $ center $ withAttr blueAttr $ drawTm val
      helpView = bordered "Help" $ center $ txt [text|
        h - backward
        l - forward
        q - quit
        ? - toggle help
      |]
  in if showHelp' then mainView <=> helpView else mainView

drawCtx :: TmShow a => [StackFrame a] -> Widget ()
drawCtx = \case
  []    -> fill ' '
  stack -> vBox (reverse $ fmap drawStackFrame stack)

drawStackFrame :: TmShow a => StackFrame a -> Widget ()
drawStackFrame = hBox . \case
  EvalFrame    k v -> [str "eval " <+> txt k <+> str "; " <+> drawTm v]
  BindingFrame k v -> [txt k <+> str ": " <+> drawTm v]

-- TODO: distinguish between in and out
drawFocus :: TmShow a => Focus a -> Widget ()
drawFocus = \case
  Descending tm -> drawTm tm
  Ascending  tm -> drawTm tm

showTermSlot :: TmShow a => Term a -> Widget ()
showTermSlot (Fix tm) = case tm of
  Term name _ -> txt $ "[" <> name <> "]"
  Var name    -> txt name
  Binding _ _ -> txt "TODO: binding"
  PrimValue a -> txt "{" <+> drawPrim a <+> txt "}"

drawBinding :: (Text, Term a) -> Widget ()
drawBinding _ = txt "binding"

drawTm :: TmShow a => Term a -> Widget ()
drawTm (Fix tm) = case tm of
  Term name subtms ->
    txt name
    <=>
    padLeft (Pad 2) (vBox (fmap drawTm subtms))
  Binding names subterm ->
    txt ("[" <> T.unwords names <> "]")
    <=>
    padLeft (Pad 2) (drawTm subterm)
  Var name   -> txt name
  PrimValue primVal -> txt "{" <+> drawPrim primVal <+> txt "}"

app :: TmShow s => App (State s) a ()
app = B.App
 { appDraw         = pure . drawUI
 , appChooseCursor = neverShowCursor
 , appHandleEvent  = handleEvent
 , appStartEvent   = return
 , appAttrMap      = const theMap
 }
