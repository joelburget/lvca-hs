{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Linguist.Brick where

import           Brick
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import           Brick.Widgets.Center       (center)
import           Control.Lens
import           Control.Zipper
import           Data.Foldable              (toList)
import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text)
import qualified Graphics.Vty               as V

import           Linguist.SimpleExample     (T)
import           Linguist.Types

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
  :: State
  -> BrickEvent () a
  -> EventM () (Next State)
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ prev g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ next g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ next g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ prev g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ prev g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ next g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ next g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ prev g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

drawUI :: State -> Widget ()
drawUI state = case state ^. focus of
  StateStep ctx valTm ->
    let lBox = bordered "Context" $ center $ drawCtx ctx
        rBox = bordered "Term"    $ center $ drawTm valTm
    in lBox <+> rBox
  Errored info -> withAttr redAttr $ center $ txt "error " <+> txt info
  Done val     -> bordered "Done"  $ center $ withAttr blueAttr $ drawVal val

drawCtx :: [StackFrame T] -> Widget ()
drawCtx = \case
  []    -> fill ' '
  stack -> vBox (reverse $ fmap drawStackFrame stack)

drawStackFrame :: StackFrame T -> Widget ()
drawStackFrame = \case
  CbvFrame name before after _ ->
    let slots = padLeft (Pad 1) <$>
          fmap showValSlot (toList before) ++ [str "_"] ++ fmap showTermSlot after
    in hBox $ txt name : slots
  BindingFrame bindings -> hBox $ Map.toList bindings <&> \(k, v) ->
    txt k <+> str ": " <+> either drawTm drawVal v

showValSlot :: Value T -> Widget ()
showValSlot = \case
  NativeValue name _vals -> txt name
  PrimValue a -> str (either show show a)
  Thunk tm -> showTermSlot tm

showTermSlot :: Term T -> Widget ()
showTermSlot = \case
  Term name _ -> txt $ "[" <> name <> "]"
  Var name -> txt name
  PrimTerm a -> str (either show show a)
  Return val -> showValSlot val

drawBinding :: (Text, Term T) -> Widget ()
drawBinding _ = txt "binding"

drawTm :: Term T -> Widget ()
drawTm = \case
  Term name subterms ->
    txt name
    <=>
    padLeft (Pad 2) (vBox (fmap drawTm subterms))
  Var name    -> txt name
  PrimTerm a -> str (either show show a)
  Return val -> str "<" <+> drawVal val <+> str ">"

drawVal :: Value T -> Widget ()
drawVal = \case
  NativeValue name vals ->
    txt name
    <=>
    padLeft (Pad 2) (vBox (fmap drawVal vals))
  PrimValue primVal -> str $ either show show primVal
  Thunk tm -> str "{" <+> drawTm tm <+> str "}"

app :: App State a ()
app = App
 { appDraw         = pure . drawUI
 , appChooseCursor = neverShowCursor
 , appHandleEvent  = handleEvent
 , appStartEvent   = return
 , appAttrMap      = const theMap
 }
