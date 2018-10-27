module Haladdin.Controller where

import           Control.Lens
import qualified Data.Foldable as F
import           Data.Geometry.Point
import           Haladdin.Action
import           Haladdin.Model
import           Miso

update     :: Action -> Model -> Effect Action Model
update a m = case a of
                    NoOp            -> noEff m
                    Time t          -> noEff m
                    GetKeysState ks -> noEff $ m&_Playing.keysState      .~ ks
                                                &_Playing.player.aladdin %~ updateAladdin ks

type PlayingAction = Action

updatePlaying :: PlayingAction -> GameState -> Effect PlayingAction GameState
updatePlaying a gs = case a of
    NoOp            -> noEff gs
    Time t          -> noEff gs
    GetKeysState ks -> noEff $ gs&keysState .~ ks


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
moveLeft a = case a^.orientation of
               Left'  -> a&position.xCoord -~ xDelta
               Right' -> a&orientation .~ Left'

moveRight   :: Aladdin -> Aladdin
moveRight a = case a^.orientation of
                Left'  -> a&orientation .~ Right'
                Right' -> a&position.xCoord +~ xDelta

-- | Only perform the action if the key is pressed
ifPressed   :: (a -> a) -> KeyState -> (a -> a)
ifPressed f = \case
                Pressed  -> f
                Released -> id
