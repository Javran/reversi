{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
module Game.Reversi.Ui
  ( app
  , initAppState
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Applicative
import Data.List
import Data.Maybe
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Input.Events
import Lens.Micro.Platform

import qualified Data.Map.Strict as M

import Game.Reversi.GameState

{-
  TODO:

  - render stuff in board.

 -}

data RName
  = RBoard
  deriving (Eq, Ord)

data UiBoard
  = UiBoard
    { _bdFocus :: Coord
    , _bdGameState :: GameState
    }

makeLenses ''UiBoard

data AppState
  = AppState
    { _uiBoard :: UiBoard
    }

makeLenses ''AppState

type ReversiApp e = App AppState e RName

vhLimit :: Int -> Int -> Widget a -> Widget a
vhLimit v h = vLimit v . hLimit h

toWidgetPos :: Int -> Int -> Coord -> Location
toWidgetPos v h (r,c) = Location (2 + c*(h+1), 2 + r*(v+1))

{-
  TODO:

  - render dark, light counts
  - status for who's turn
  - status for game concluded
  - status for forced skip

 -}
widgetStatus :: AppState -> Widget RName
widgetStatus _s =
    border $ vLimit 1 $ darkCount <+> vBorder <+> lightCount <+> vBorder <+> statusText
  where
    darkCount =
      str "X:" <+>
        (hLimit 3 . padLeft Max $ str "??")
    lightCount =
      str "O:" <+>
        (hLimit 3 . padLeft Max $ str "??")
    statusText = str "TODO: status here."

widgetBoard :: AppState -> Widget RName
widgetBoard s =
    border $ padAll 1 $ joinBorders
      $ showCursor RBoard (toWidgetPos v h focus)
      $ vhLimit fullV fullH grid
  where
    (v,h) = (1,1)
    AppState (UiBoard focus gs) = s
    (fullV, fullH) = ((v+1)*8+1, (h+1)*8+1)
    grid = center $ vBox (intersperse hBorder $ firstRow : rows)
      where
        firstRow = vLimit 1 $
          center $ hBox $ intersperse vBorder $
            tl : (hLimit h . center . str <$> (show <$> [1 :: Int .. 8]))
        tl = vhLimit 1 1 $ fill ' '
        rows = zipWith mkRow [0..] ['a'..'h']
    mkRow rowInd hd =
        center $ hBox (intersperse vBorder (hdW : fmap (mkCell rowInd) [0..7]))
      where
        hdW = vhLimit v 1 $ center $ str [hd]
    mkCell :: Int -> Int -> Widget RName
    mkCell rowInd colInd =
        vhLimit v h $ center
          $ str [fromMaybe ' ' (chTaken <|> chPossible)]
      where
        coord = (rowInd, colInd)
        chTaken = do
          color <- gsBoard gs M.!? coord
          pure $  if color then 'X' else 'O'
        chPossible = do
          Right m <- pure (possibleMoves gs)
          _ <- m M.!? coord
          pure '?'

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
        continue $ (uiBoard . bdFocus %~ move) s
    VtyEvent (EvKey k [])
      | k `elem` [KEnter, KChar ' '] ->
          let gs = s ^. uiBoard . bdGameState
              focus = s ^. uiBoard . bdFocus
          in case possibleMoves gs of
            Left _ ->
              -- force to skip
              let Just gs' = switchSide gs
              in continue $ s & uiBoard . bdGameState .~ gs'
            Right m -> case m M.!? focus of
              -- apply a valid move
              Just _ ->
                let Just gs' = applyMove gs focus
                in continue $ s & uiBoard . bdGameState .~ gs'
              Nothing -> continue s
    VtyEvent (EvKey (KChar 'r') []) ->
      let gs = s ^. uiBoard . bdGameState
      in continue $ if gameConcluded gs then initAppState else s
    VtyEvent (EvKey (KChar '\t') []) ->
      let gs = s ^. uiBoard . bdGameState
          focus = s ^. uiBoard . bdFocus
          orderedPossibleMoves = case possibleMoves gs of
            Left _ -> []
            Right m -> M.keys m
          nextMoves =
            zip
              orderedPossibleMoves
              (let hd:tl = orderedPossibleMoves in tl <> [hd])
      in continue $ case lookup focus nextMoves of
        Nothing ->
          case orderedPossibleMoves of
            [] -> s
            hd:_ -> s & uiBoard . bdFocus .~ hd
        Just c' -> s & uiBoard . bdFocus .~ c'
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
    appDraw s = [center $ hCenter (widgetBoard s) <=> hCenter (widgetStatus s)]
    appHandleEvent = handleEvent
    appAttrMap _ = attrMap defAttr []
    appChooseCursor _ = showCursorNamed RBoard

initAppState :: AppState
initAppState = AppState (UiBoard (0,0) initGameState)
