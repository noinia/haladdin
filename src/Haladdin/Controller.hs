module Haladdin.Controller where

import           Control.Lens hiding (element)
import qualified Data.Foldable as F
import           Data.Geometry.Point
import           Data.Geometry.Vector
import           Haladdin.Action
import           Haladdin.Model
import           Miso


update     :: Action -> Model -> Effect Action Model
update a m = case a of
               StartGame        -> noEff m -- startGame
               TogglePause      -> togglePause m
               PlayingAction pa -> case m of
                                     Playing gs -> bimap PlayingAction Playing $
                                                     updatePlaying pa gs
                                     _          -> noEff m

startGame :: Effect Action GameMode
startGame = bimap PlayingAction Playing $ step initialGameState

togglePause :: GameMode -> Effect Action GameMode
togglePause = \case
    Playing gs -> noEff $ Paused gs
    Paused gs  -> bimap PlayingAction Playing $ step gs
    m          -> noEff m

-- type PlayingAction = Action

updatePlaying :: PlayingAction -> GameState -> Effect PlayingAction GameState
updatePlaying a gs = case a of
    NoOp            -> noEff gs
    Time t          -> noEff gs
    GetKeysState ks -> noEff $ gs&keysState .~ ks
                                 &player.aladdin %~ updateAladdin ks



step    :: GameState -> Effect PlayingAction GameState
step gs = noEff gs --gs&player.aladdin %~ updateAladdin



  -- k <# do Time <$> now
  -- where
  --   k = gs&player.aladdin %~ gravity (gs^.deltaTime)

gravity      :: Double -> Aladdin -> Aladdin
gravity dt a = a

  -- =
  -- m { vy = if y > 0 then vy - (dt / 4) else 0 }



updateAladdin      ::  KeysState -> Aladdin -> Aladdin
updateAladdin ks a = F.foldl' (\a' f -> f a') a  update'
  where
    update' :: GKeysState (Aladdin -> Aladdin)
    update' = update'' <*> ks
    -- TODO: jump slash etc.

    update'' :: GKeysState (KeyState -> Aladdin -> Aladdin)
    update'' = KeysState  (ifPressed moveLeft)
                          (ifPressed moveRight)
                          (const id) -- up
                          crouch
                          (ifPressed jump)
                          (const id) -- throw
                          (const id) -- slash

    crouch = \case
               Pressed  -> (&movementState .~ Crouching)
               Released -> (&movementState .~ Standing)


    jump = case ks^.downKey of
             Pressed  -> id -- cannot jump if we are also pressing key down
             Released -> (\a' -> a'&movementState   .~ Jumping
                                   &position.yCoord +~ yDelta
                         )


xDelta = 5
yDelta = 5

-- if we are looking in the right direction yet; move in that direction
moveLeft    :: Aladdin -> Aladdin
moveLeft a | a^.velocity.xComponent <= 1 = a&position.xCoord     -~ xDelta
           | otherwise                   = a&velocity.xComponent .~ 0

moveRight   :: Aladdin -> Aladdin
moveRight a | a^.velocity.xComponent >= (-1) = a&position.xCoord     +~ xDelta
            | otherwise                      = a&velocity.xComponent .~ 0


-- | Only perform the action if the key is pressed
ifPressed   :: (a -> a) -> KeyState -> (a -> a)
ifPressed f = \case
                Pressed  -> f
                Released -> id



xComponent :: Lens' (Vector 2 r) r
xComponent = element (C :: C 0)

yComponent :: Lens' (Vector 2 r) r
yComponent = element (C :: C 1)
