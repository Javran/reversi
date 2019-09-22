{-# LANGUAGE RecordWildCards #-}
module Game.Reversi.GameState
  ( GameState
  , Coord
  , Color
  , Board
  , PossibleMoves
  , initGameState
  , applyMove
  , possibleMoves
  , gsBoard
  , gsTurn
  , gsNextMoves
  , gameConcluded
  , switchSide
  ) where

import Data.Bifunctor
import Data.Bool

import qualified Data.Map.Strict as M
import qualified Data.Set as S

import qualified Game.Reversi.Core as Core
import Game.Reversi.Core
  ( Coord
  , Color
  , Board
  , allCoords
  , initBoard
  )

{-
  This module defines GameState and operations around it.
 -}

data GameState
  = GameState
  { gsBoard :: Board
  , gsFreeCells :: S.Set Coord -- not yet occupied cells
  , gsTurn :: Color -- who's turn
  , gsNextMoves :: (PossibleMoves, PossibleMoves)
    -- (<next possible moves for light>, <next possible moves for dark>)
  }

{-
  While it is true that we can just use a Map to check
  whether there are possible next moves, Map needs to visit all its keys
  for its emptiness check (e.g. `M.null (M.fromList [1..])` takes forever
  despite that the expected result is obvious),
  The solution here is to return possibilities using list
  and scrutiny on that to return either Left and Right.

  And of course this is an over-engineering on performance - it's too boring
  to just implement reversi without some.
 -}
type PossibleMoves
  = Either
      {-
        Left b means that no move is possible, player must switch side.
        (b is the same board with no change at all).
       -}
      Board
      {-
        Right m means that at least one move is possible,
        and of course m must be non-empty.
       -}
      (M.Map Coord Board)

initGameState :: GameState
initGameState = GameState {..}
  where
    gsBoard = initBoard
    gsFreeCells = S.difference (S.fromList allCoords) (M.keysSet gsBoard)
    gsTurn = True -- dark always moves first.
    gsNextMoves = bimap pm pm (False, True)
      where
        pm = mkPossibleMoves (S.toAscList gsFreeCells) gsBoard

mkPossibleMoves :: [] Coord -> Board -> Color -> PossibleMoves
mkPossibleMoves cs bd who = case Core.possibleMoves cs bd who of
  [] -> Left bd
  moves@(_:_) -> Right (M.fromDistinctAscList moves)

possibleMoves :: GameState -> PossibleMoves
possibleMoves gs = bool fst snd (gsTurn gs) . gsNextMoves $ gs

gameConcluded :: GameState -> Bool
gameConcluded GameState { gsNextMoves = (Left _, Left _) } = True
gameConcluded _ = False

applyMove :: GameState -> Coord -> Maybe GameState
applyMove gs coord = do
  let who = gsTurn gs
  Right nextMoves <- pure (possibleMoves gs)
  bd' <- nextMoves M.!? coord
  let freeCells = S.delete coord (gsFreeCells gs)
  pure GameState
    { gsBoard = bd'
    , gsFreeCells = freeCells
    , gsTurn = not who
    , gsNextMoves =
        let pm = mkPossibleMoves (S.toList freeCells) bd'
        in bimap pm pm (False, True)
    }

switchSide :: GameState -> Maybe GameState
switchSide gs = do
  Left bd <- pure (possibleMoves gs)
  pure (gs {gsTurn = not (gsTurn gs), gsBoard = bd})
