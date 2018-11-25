module Haladdin.Action where

import Haladdin.Model
import Miso
-- import Miso.Event.Types

-- data Action = HandleKey KeyCode

data Action
  = StartGame
  | TogglePause
  | GetKeysState !KeysState  -- ^ Some keyboard input
  | Step !Time               -- ^ The time at which step was called.
