{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
import qualified Data.Text                  as T
import           Data.Void                  (Void, absurd)
import qualified Graphics.Vty               as V

import           Linguist.SimpleExample     (E)
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
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

drawUI :: TmShow s => State s -> Widget ()
drawUI state = case state ^. focus of
  StateStep ctx valTm ->
    let lBox = bordered "Context" $ center $ drawCtx ctx
        rBox = bordered "Term"    $ center $ drawTmVal valTm
    in lBox <+> rBox
  Errored info -> withAttr redAttr $ center $ txt "error " <+> txt info
  Done val     -> bordered "Done"  $ center $ withAttr blueAttr $ drawVal val

drawCtx :: TmShow a => [StackFrame a] -> Widget ()
drawCtx = \case
  []    -> fill ' '
  stack -> vBox (reverse $ fmap drawStackFrame stack)

drawStackFrame :: TmShow a => StackFrame a -> Widget ()
drawStackFrame = \case
  CbvForeignFrame name before after _ ->
    let slots = padLeft (Pad 1) <$>
          fmap showValSlot (toList before) ++ [str "_"] ++ fmap showTermSlot after
    in hBox $ txt name : slots
  CbvFrame varName _body -> txt $ "evaluating arg " <> varName
  BindingFrame bindings -> hBox $ Map.toList bindings <&> \(k, v) ->
    txt k <+> str ": " <+> drawTmVal v

class TmShow a where
  drawPrim :: a -> Widget ()

instance TmShow E where
  drawPrim = str . either show show

instance TmShow Void where
  drawPrim = str . absurd

drawTmVal :: TmShow a => TmVal a -> Widget ()
drawTmVal = either drawTm drawVal

showValSlot :: TmShow a => Value a -> Widget ()
showValSlot = \case
  NativeValue name _vals -> txt name
  PrimValue a            -> drawPrim a
  Thunk tm               -> showTermSlot tm

showTermSlot :: TmShow a => Term a -> Widget ()
showTermSlot = \case
  Term name _ -> txt $ "[" <> name <> "]"
  Var name    -> txt name
  PrimTerm a  -> drawPrim a -- str (either show show a)
  Return val  -> showValSlot val
  Binding _ _ -> txt "TODO: binding"

drawBinding :: (Text, Term a) -> Widget ()
drawBinding _ = txt "binding"

drawTm :: TmShow a => Term a -> Widget ()
drawTm = \case
  Term name subtms ->
    txt name
    <=>
    padLeft (Pad 2) (vBox (fmap drawTm subtms))
  Binding names subterm ->
    txt ("[" <> T.unwords names <> "]")
    <=>
    padLeft (Pad 2) (drawTm subterm)
  Var name    -> txt name
  PrimTerm a -> drawPrim a -- str (either show show a)
  Return val -> str "<" <+> drawVal val <+> str ">"

drawVal :: TmShow a => Value a -> Widget ()
drawVal = \case
  NativeValue name vals ->
    txt name
    <=>
    padLeft (Pad 2) (vBox (fmap drawVal vals))
  PrimValue primVal -> drawPrim primVal -- str $ either show show primVal
  Thunk tm -> str "{" <+> drawTm tm <+> str "}"

app :: TmShow s => App (State s) a ()
app = App
 { appDraw         = pure . drawUI
 , appChooseCursor = neverShowCursor
 , appHandleEvent  = handleEvent
 , appStartEvent   = return
 , appAttrMap      = const theMap
 }
