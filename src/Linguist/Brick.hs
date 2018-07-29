{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Linguist.Brick where

import           Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import           Control.Lens
import           Control.Zipper
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Graphics.Vty    as V

import Linguist.Types

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

handleEvent :: State -> BrickEvent () a -> EventM () (Next State)
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
  StateStep ctx tm ->
    let lBox = bordered "Context" $ drawCtx ctx
        rBox = bordered "Term" $ drawTm tm
    in lBox <+> rBox
  Errored -> withAttr redAttr $ txt "error"
  Done tm -> bordered "Done" $ withAttr blueAttr $ drawTm tm

drawCtx :: EvalContext (Either Int String) -> Widget ()
drawCtx (EvalContext stack bindings) =
  vBox (fmap drawStackFrame stack)
  <=>
  hBox (fmap drawBinding (Map.toList bindings))

drawStackFrame :: HoleyTerm (Either Int String) -> Widget ()
drawStackFrame (HoleyTerm name before after) =
  let slots = padLeft (Pad 1) <$>
        (const (str "_") <$> before) ++ [str "^"] ++ (const (str "_") <$> after)
  in hBox $ txt name : slots

drawBinding :: (Text, Term (Either Int String)) -> Widget ()
drawBinding _ = txt "binding"

drawTm :: Term (Either Int String) -> Widget ()
drawTm = \case
  Term name subterms ->
    txt name
    <=>
    padLeft (Pad 2) (vBox (fmap drawTm subterms))
  Var name    -> txt name
  Primitive (Left a) -> str (show a)
  Primitive (Right a) -> str (show a)

app :: App State a ()
app = App
 { appDraw         = pure . drawUI
 , appChooseCursor = neverShowCursor
 , appHandleEvent  = handleEvent
 , appStartEvent   = return
 , appAttrMap      = const theMap
 }
