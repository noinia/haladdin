module Haladdin.View where

import Miso
import Haladdin.Model
import Haladdin.Action

viewModel :: Model -> View Action
viewModel = render

class Renderable t where
  render :: t -> View Action

instance Renderable GameMode where
  render = undefined

instance Renderable GameState where
  render = undefined
