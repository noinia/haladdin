module Haladdin.Action where

import Haladdin.Model
import Miso
-- import Miso.Event.Types

-- data Action = HandleKey KeyCode

data Action
  = GetKeysState !KeysState
  | Time !Double
  | NoOp
