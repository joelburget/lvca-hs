{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Lvca.Brick where

import           Brick                       as B
import qualified Brick.Widgets.Border        as B
import qualified Brick.Widgets.Border.Style  as BS
import           Brick.Widgets.Center        (center)
import           Control.Lens
import           Control.Zipper
import           Data.Text                   (Text)
import           Data.Void                   (Void, absurd)
import qualified Graphics.Vty                as V
import           NeatInterpolation


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

data State f a = State
  -- { _timeline :: ListZipper (StateStep f a)
  { _showHelp :: Bool
  }

makeLenses ''State

next :: State f a -> State f a
next state = state

prev :: State f a -> State f a
prev state = state

toggleHelp :: State f a -> State f a
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
  :: State f s
  -> BrickEvent () a
  -> EventM () (Next (State f s))
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

drawUI :: TmShow s => State f s -> Widget ()
drawUI (State showHelp') =

  let mainView = txt "TODO"
      helpView = bordered "Help" $ center $ txt [text|
        h - backward
        l - forward
        q - quit
        ? - toggle help
      |]
  in if showHelp' then mainView <=> helpView else mainView

-- drawCtx :: TmShow a => [StackFrame f a] -> Widget ()
-- drawCtx = \case
--   []    -> fill ' '
--   stack -> vBox (reverse $ fmap drawStackFrame stack)

-- drawStackFrame :: TmShow a => StackFrame f a -> Widget ()
-- drawStackFrame = hBox . \case
--   EvalFrame    _k _v -> [str "eval " <+> txt "TODO: k" <+> str "; " <+> txt "TODO: v"]
--   BindingFrame k v -> [txt k <+> str ": " <+> drawTm' v]

-- -- TODO: distinguish between in and out
-- drawFocus :: TmShow a => Focus f a -> Widget ()
-- drawFocus = \case
--   Descending tm -> drawTm  tm
--   Ascending  tm -> drawTm' tm

-- showTermSlot :: TmShow a => Term a -> Widget ()
-- showTermSlot tm = case tm of
--   Term name _ -> txt $ "[" <> name <> "]"
--   Var name    -> txt name
--   Binding _ _ -> txt "TODO: binding"
--   PrimValue a -> txt "{" <+> drawPrim a <+> txt "}"

app :: TmShow s => App (State f s) a ()
app = B.App
 { appDraw         = pure . drawUI
 , appChooseCursor = neverShowCursor
 , appHandleEvent  = handleEvent
 , appStartEvent   = return
 , appAttrMap      = const theMap
 }
