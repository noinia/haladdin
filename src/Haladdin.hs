module Haladdin where

import           Control.Lens hiding (view, Level)
import           Data.Ext
import           Data.Geometry.Box
import           Data.Geometry.Point
import           Data.Geometry.Vector
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
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
                    , subs          = [ keysStateSub GetKeysState ]
                    , events        = defaultEvents
                    , initialAction = NoOp
                    , mountPoint    = Nothing
                    }
    startApp myApp

--------------------------------------------------------------------------------
-- * Initial Values

initialModel :: GameMode
initialModel = Playing $ GameState initialWorld initialPlayer initialViewPort initialKeysState

initialWorld :: World
initialWorld = World [] (NonEmpty.head allLevels) allLevels

allLevels :: NonEmpty Level
allLevels = NonEmpty.fromList [ Level [] [] [] $ box (ext $ Point2 10 10) (ext $ Point2 11 11)
                              ]

initialPlayer :: Player
initialPlayer =
    Player initialAladin initialLives 0 (Score 0) initialRespawnPoint

initialRespawnPoint :: RespawnPoint
initialRespawnPoint = RespawnPoint origin

initialViewPort :: ViewPort
initialViewPort = let w = 800
                      h = 600
                  in ViewPort (Point2 (w/2) (h/2)) (Vector2 w h)

initialKeysState :: KeysState
initialKeysState = allKeys Released

initialAladin :: Aladdin
initialAladin = Aladdin (initialRespawnPoint^.respawnPoint)
                        Right'
                        Standing
                        initialApples
                        Shielded
                        initialHealth

--------------------------------------------------------------------------------
-- * Some constants

initialLives :: Count
initialLives = 3

initialHealth :: Health
initialHealth = Health 100

initialApples :: Count
initialApples = 10
