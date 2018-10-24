module Haladdin.Action where

import Miso
-- import Miso.Event.Types

-- data Action = HandleKey KeyCode

data Action
  = HandleMouse (Int, Int)
  | Id
