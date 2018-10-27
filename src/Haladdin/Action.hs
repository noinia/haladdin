module Haladdin.Action where

import Haladdin.Model
import Miso
-- import Miso.Event.Types

-- data Action = HandleKey KeyCode

data Action
  = StartGame
  | TogglePause
  | PlayingAction !PlayingAction
-- type Action = PlayingAction

data PlayingAction
  = GetKeysState !KeysState
  | Time !Double
  | NoOp
