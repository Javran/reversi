module Game.Reversi.Core
  ( Coord
  , Color
  , Board
  , allCoords
  , initBoard
  , dirs
  , getDisks
  , applyMoveOnDir
  , applyMove
  , possibleMoves
  ) where

import Control.Monad
import Data.Maybe

import qualified Data.Map.Strict as M
import qualified Data.Set as S

{-
  Core game logic of Reversi.

  This module is supposed to define no data type
  and work on well-known data structures only.

  For manipulation on GameState, use Game.Reversi.GameState
  and avoid directly importing this module.
  (as there will be intentional name conflicts)
 -}

type Coord = (Int {- row -}, Int {- col -}) -- note that this is 0-based index
type Color = Bool -- False for light, True for dark

type Board = M.Map Coord Color

allCoords :: [] Coord
allCoords = [ (r,c) | r <- [0..7], c <- [0..7] ]

initBoard :: Board
initBoard = M.fromList [((3,3),False), ((4,4),False), ((3,4),True), ((4,3),True)]

type Dir = (Int, Int)

dirs :: [] Dir
dirs = ds <> ((\(r,c) -> (-r,-c)) <$> ds)
  where
    ds = [(1,0),(0,1),(1,1),(1,-1)]

-- get the list of disks starting from coord, along dir.
getDisks :: Board -> Coord -> Dir -> [(Coord, Maybe Color)]
getDisks bd coord (dr,dc) = (\k -> (k, M.lookup k bd)) <$> coords
  where
    coords = iterate (\(r,c) -> (r+dr,c+dc)) coord

-- get the list of coordinates of disks that will be filpped
-- because of next moving being coord :: Coord.
applyMoveOnDir :: Board -> Color -> Coord -> Dir -> [Coord]
applyMoveOnDir bd who coord dir =
    if null owns
      then []
      else fst <$> oppos
  where
    (oppos, owns) = span ((/= who) . snd) consecutives
    consecutives =
      fmap (\(k, Just v) -> (k, v))
      -- only take consecutive non-empty values
      . takeWhile (isJust . snd)
      -- skip first element, which is coord itself.
      . tail $ getDisks bd coord dir

applyMove :: Board -> Color -> Coord -> Maybe (S.Set Coord, Board)
applyMove bd who coord = do
  guard $ M.notMember coord bd
  let flipCoords = dirs >>= \dir -> applyMoveOnDir bd who coord dir
      flipCoordsSet = S.fromList flipCoords
      bd' =
        S.foldr
          (\coord' -> M.insert coord' who)
          (M.insert coord who bd)
          flipCoordsSet
  guard . not . null $ flipCoords
  pure (flipCoordsSet, bd')

possibleMoves :: [] Coord -> Board -> Color -> [] (Coord, Board)
possibleMoves coords bd who =
  foldr
    (\c acc -> [ (c,bd') | Just (_, bd') <- [applyMove bd who c] ] <> acc)
    []
    coords
