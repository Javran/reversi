module Game.Reversi.Ui where

import Game.Reversi.GameState

data UiBoard
  = UiBoard
    { _bdFocus :: (Int, Int) -- (row, col)
    , _bdGameStae :: GameState
    }

data AppState
  = AppState
    { _uiBoard :: ()
    }
