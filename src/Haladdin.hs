{-# LANGUAGE RecordWildCards            #-}
module Haladdin where

import qualified Data.Map as Map
import           Haladdin.Controller
import           Haladdin.Model
import           Haladdin.View
import qualified Language.Javascript.JSaddle.Warp as JSaddle
import           Miso


main :: IO ()
main = JSaddle.run 8080 $ do
    startApp App { .. }
  where
    initialAction = Id
    model         = emptyModel
    update        = updateModel
    view          = viewModel
    events        = Map.insert (pack "mousemove") False $
                    Map.insert (pack "touchstart") False $
                    Map.insert (pack "touchmove") False defaultEvents
    subs          = [ mouseSub HandleMouse ]
    mountPoint    = Nothing


initialModel = Playing $ GameState initialWorld initialPlayer initialViewPort

initialWorld = World [] initialLevel allLevels

initialLevel = undefined -- Level

initialPlayer = Player initialAladin initialLives 0 (Score 0) Shielded initialHealth

initialLives :: Count
initialLives = 3

initialHealth :: Health
initialHealth = Health 100


initialViewPort = ViewPort origin (Vector2 800 600)

initialAladin = undefined
