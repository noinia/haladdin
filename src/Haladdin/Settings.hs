module Haladdin.Settings where


import Data.Geometry.Vector
import Data.Range
import Haladdin.Model

maxVelocity :: Vector 2 R
maxVelocity = Vector2 5 2


xDelta = 1


-- | Speed with which we can jump
jumpVelocity :: R
jumpVelocity = 6


-- | Speed with which we start falling
fallingSpeed :: R
fallingSpeed = -6
