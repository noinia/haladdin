module Haladdin where

import           Haladdin.Action
import           Haladdin.Input
import qualified Haladdin.Controller as Controller
import           Haladdin.Model
import qualified Haladdin.View as View
import qualified Language.Javascript.JSaddle.Warp as JSaddle
import           Miso

--------------------------------------------------------------------------------

main :: IO ()
main = JSaddle.run 8080 $ do
    let myApp = App { model         = initialModel
                    , update        = Controller.update
                    , view          = View.view
                    , subs          = [ keysStateSub $ PlayingAction . GetKeysState ]
                    , events        = defaultEvents
                    , initialAction = StartGame
                    , mountPoint    = Nothing
                    }
    startApp myApp
