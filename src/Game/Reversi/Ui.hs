{-# LANGUAGE RecordWildCards #-}
module Game.Reversi.Ui where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Data.List
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Input.Events

import Game.Reversi.GameState

{-
  TODO:

  - render stuff in board.
  - use lens utils rather than those manual field mods.

 -}

data RName = RName deriving (Eq, Ord)

data UiBoard
  = UiBoard
    { _bdFocus :: Coord
    , _bdGameStae :: GameState
    }

data AppState
  = AppState
    { _uiBoard :: UiBoard
    }

type ReversiApp e = App AppState e RName

vhLimit :: Int -> Int -> Widget a -> Widget a
vhLimit v h = vLimit v . hLimit h

toWidgetPos :: Int -> Int -> Coord -> Location
toWidgetPos v h (r,c) = Location (2 + c*(h+1), 2 + r*(v+1))

ui :: Int -> Int -> AppState -> Widget RName
ui v h s =
  center
    $ border $ padAll 1 $ joinBorders
    $ showCursor RName (toWidgetPos v h coord)
    $ vhLimit fullV fullH grid
  where
    AppState (UiBoard coord _) = s
    (fullV, fullH) = ((v+1)*8+1, (h+1)*8+1)
    grid = center $ vBox (intersperse hBorder $ firstRow : rows)
      where
        firstRow = vLimit 1 $
          center $ hBox $ intersperse vBorder $
            tl : (hLimit h . center . str <$> (show <$> [1 :: Int ..8]))
        tl = vhLimit 1 1 $ fill ' '
        rows = row <$> ['a'..'h']
    row hd = center $ hBox (intersperse vBorder (hdW : replicate 8 cell))
      where
        hdW = vhLimit v 1 $ center $ str [hd]
    cell = vhLimit v h $ center $ str "#"

clamped :: (Coord -> Coord) -> (Coord -> Coord)
clamped f s
  | r >= 0 && r < 8 && c >= 0 && c < 8 = s'
  | otherwise = s
  where
    s'@(r,c) = f s

handleEvent :: AppState -> BrickEvent RName e -> EventM RName (Next AppState)
handleEvent s e = case e of
    VtyEvent (EvKey k [])
      | Just move <- keyMove k ->
        let AppState (UiBoard c gs) = s
        in continue $ s { _uiBoard = UiBoard (move c) gs }
    VtyEvent (EvKey (KChar 'q') []) -> halt s
    _ -> continue s

keyMove :: Key -> Maybe (Coord -> Coord)
keyMove KLeft = pure $ clamped $ \(r,c) -> (r,c-1)
keyMove KRight = pure $ clamped $ \(r,c) -> (r,c+1)
keyMove KUp = pure $ clamped $ \(r,c) -> (r-1,c)
keyMove KDown = pure $ clamped $ \(r,c) -> (r+1,c)
keyMove _ = Nothing

app :: ReversiApp a
app = App {appStartEvent = pure, ..}
  where
    appDraw s = [ui 1 1 s]
    appHandleEvent = handleEvent
    appAttrMap _ = attrMap defAttr []
    appChooseCursor _ = showCursorNamed RName
