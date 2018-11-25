module Haladdin.Controller where

import           Control.Concurrent
import           Control.Lens hiding (element, Level)
import           Control.Monad.IO.Class
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Box
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Vector
import           Data.List (genericLength)
import           Data.Maybe
import           Data.Ord (comparing)
import           Data.Range
import           Data.UnBounded
import           Haladdin.Action
import           Haladdin.Model
import           Miso hiding (set)
--------------------------------------------------------------------------------

update     :: Action -> Model -> Effect Action Model
update a m = case a of
               StartGame       -> startGame
               TogglePause     -> togglePause m
               GetKeysState ks -> noEff $ case m of
                                    Playing gs -> Playing $ gs&keysState .~ ks
                                    _          -> m
               Step t          -> case m of
                                    Playing gs -> step t gs
                                    _          -> noEff m
                                                  -- no longer playing, so stop stepping

--------------------------------------------------------------------------------

startGame :: Effect Action GameMode
startGame = step 0 initialGameState

togglePause :: GameMode -> Effect Action GameMode
togglePause = \case
    Playing gs -> noEff $ Paused gs
    Paused gs  -> step (gs^.gameTime) gs
    m          -> noEff m

-- | The main update function that does a playing action.
step      :: Time -> GameState -> Effect Action Model
step t gs = progressTime t gs <# do Step <$> now
  -- when we get a step event with time t, we compute a new gamestate using
  -- the 'step' function. Moreover, we generate a new step event at the current time.

progressTime                   :: Time -> GameState -> Model
progressTime t gs | hasWon     = Finished gameScore
                  | hasDied    = GameOver gameScore
                  | otherwise  = Playing gs'
  where
    -- we basically do two things: we move everything that can move, and then
    -- apply all game rules/gamelogic
    gs' = (\gs'' -> applyLogic gs'' gs'')
        . progressTimeWith gs (t - gs^.gameTime)
        $ gs
    gameScore = gs'^.player.score
    hasWon  = False -- todo
    hasDied = gs^.player.aladdin.health == Health 0 && gs^.player.lives == 0

--------------------------------------------------------------------------------

class Progressable t where
  -- | Given the gamestate and a dt value (i.e. how much time has passed since
  -- gametime), update the t.
  progressTimeWith :: GameState -> Time -> t -> t

class ApplyLogic t where
  -- | perform whatever update to this t
  applyLogic :: GameState -> t -> t

class ApplyLogicDeletable t where
  -- | perform whatever update to this t (in case the type may dissapear)
  applyLogic' :: GameState -> t -> Maybe  t

--------------------------------------------------------------------------------
-- * Everything that moves/changes as a function of time.

instance Progressable GameState where
  progressTimeWith _ dt gs = gs&world    %~ progressTimeWith gs dt
                               &player   %~ progressTimeWith gs dt
                               &gameTime +~ dt

instance Progressable World where
  progressTimeWith gs dt w = w&activeLevel %~ progressTimeWith gs dt

instance Progressable Level where
  progressTimeWith gs dt l = l&enemies.traverse %~ progressTimeWith gs dt

instance Progressable Enemy where
  progressTimeWith gs dt e = e -- TODO

instance Progressable Player where
  progressTimeWith gs dt p = p&aladdin %~ progressTimeWith gs dt

instance Progressable Aladdin where
  progressTimeWith gs dt a = a&position      %~ move'          dt (a^.velocity) ground
                              &velocity      %~ updateVelocity dt ks canJump'
                              &movementState %~ movementState' ks
    where
      ks = gs^.keysState
      ground = groundAt' (gs^.world.activeLevel)

      canJump' = canJump (a^.movementState)

-- | Compute a new movement state, for now just keep whatever it was
movementState' ks ms = ms

-- | Moves aladin
move'               :: Time -> Vector 2 R -> (Point 2 R -> R) -> Point 2 R -> Point 2 R
move' dt v ground p = p&xCoord .~ x
                       &yCoord .~ gp `max` (p^.yCoord + dt*v^.yComponent)
  where
    x  = p^.xCoord + dt*v^.xComponent
    gp = ground p

maxVelocity :: Vector 2 R
maxVelocity = Vector2 5 2

-- | Computes the new velocity of aladdin
updateVelocity                          :: Time -> KeysState
                                        -> Bool -- ^ can we currently jump
                                        -> Vector 2 R -> Vector 2 R
updateVelocity dt ks cj (Vector2 vx vy) = Vector2 clamp' clamp' <*> speedInterval
                                                                <*> Vector2 vx' vy'
  where
    vx' = degrade dt vx + (ifPressed (ks^.leftKey)  $ (-1)*xDelta)
                        + (ifPressed (ks^.rightKey) $ xDelta)
    vy' = degrade dt vy + (if cj then ifPressed (ks^.jumpKey) jumpVelocity else 0)


    ifPressed Pressed  x = x
    ifPressed Released _ = 0

    speedInterval :: Vector 2 (Range R)
    speedInterval = (\z -> ClosedRange ((-1)*z) z) <$> maxVelocity

    clamp' i = trunc . clampTo i

    -- for very small values just reset to zero
    trunc x | abs x < 0.01 = 0
            | otherwise    = x

-- slows down some value depending on how much time has progressed
degrade dt x = dt*0.005*x





-- move           :: Level -> KeysState -> Time -> Aladdin -> Aladdin
-- move lvl ks dt = aladdinPhysics  dt ground (lvl^.bBox)
--                . applyKeysState  dt ks
--                . aladdinGravity  dt ground
--   where
--     ground = groundAt' lvl


--------------------------------------------------------------------------------
-- * Applying Game Logic

instance ApplyLogic GameState where
  applyLogic _ gs = gs&world    %~ applyLogic gs
                      &player   %~ applyLogic gs
                      &viewPort %~ applyLogic gs

instance ApplyLogic World where
  applyLogic gs w | levelCompleted = w -- TODO; progress a level
                  | otherwise      = w&activeLevel %~ applyLogic gs
    where
      levelCompleted = (gs^.player.aladdin.position)
                       `intersects`
                       (w^.activeLevel.target.to boundingBox)

instance ApplyLogic Level where
  applyLogic gs l = l&collectables %~ mapMaybe (applyLogic' gs)
                     &enemies      %~ mapMaybe (applyLogic' gs)

instance ApplyLogicDeletable Collectable where
  applyLogic' gs c = case c^.collectableKind of
     SavePoint                                                    -> Just c
     _ | (gs^.player.aladdin.position) `intersects` boundingBox c -> Nothing
       | otherwise                                                -> Just c

instance ApplyLogicDeletable Enemy where
  applyLogic' _ e | e^.health <= Health 0 = Nothing
                  | otherwise             = Just e

-- | Number of points you get for collecting a ruby
rubyScore :: Score
rubyScore = 10

instance ApplyLogic Player where
  applyLogic gs p = p&aladdin         %~ applyLogic gs
                     &rubies          +~ nr
                     &respawnLocation %~ latestRespawnLoc pos cs
                     &score           +~ fromIntegral nr * rubyScore
    where
      cs  = gs^.world.activeLevel.collectables
      pos = p^.aladdin.position
      nr  = collected CollectableRuby pos cs

latestRespawnLoc pos cs rl = rl

-- | figure out how many rubies we have collected
collected       :: CollectableKind -> Point 2 R ->  [Collectable] -> Count
collected k pos = genericLength . filter p
  where
    p c = (c^.collectableKind) == k && pos `intersects` (boundingBox c)



instance ApplyLogic Aladdin where
  applyLogic gs a = a&apples +~ collected CollectableApple pos cs
    where
      cs  = gs^.world.activeLevel.collectables
      pos = a^.position

instance ApplyLogic ViewPort where
  applyLogic gs = tryCenterViewPort (gs^.player.aladdin.position)
                                    (gs^.world.activeLevel)

tryCenterViewPort          :: Point 2 R -> Level -> ViewPort -> ViewPort
tryCenterViewPort p lvl vp = vp&viewPortCenter .~ limitTo rect p
  where
    rect = shrink ((/2) <$> vp^.viewPortDimensions) $ boundingBox lvl














shrink     :: Num r => Vector 2 r -> Rectangle p r -> Rectangle p r
shrink v r = r&minP.core.cwMin %~ (.+^ v)
              &maxP.core.cwMax %~ (.-^ v)
  -- TODO: if the level is too small this may not work i guess.


clip                              :: Ord r
                                  => Vector 2 (Range r) -> Point 2 r -> Point 2 r
clip (Vector2 xr yr) (Point2 x y) = Point2 (clip' x xr) (clip' y yr)
  where
    clip' a (Range' l u) = (a `min` u) `max` l

-- | Limit the point to stay inside the rectangle
limitTo   :: (Ord r, Num r) => Rectangle p r -> Point 2 r -> Point 2 r
limitTo r = clip (extent r)

--------------------------------------------------------------------------------
-- * Dealing with input

applyKeysState         :: Time -> KeysState -> Aladdin -> Aladdin
applyKeysState dt ks a = F.foldl' (\a' f -> f a') a  $ update' <*> ks
  where
    update' :: GKeysState (KeyState -> Aladdin -> Aladdin)
    update' = KeysState  (ifPressed' moveLeft $ slowDown dt)
                         (ifPressed' moveRight $ slowDown dt)
                         (ifPressed' crouch unCrouch)
                         (const id) -- up
                         (ifPressed jump)
                         (const id) -- throw
                         (ifPressed' drawSword shieldSword) -- slash

slowDown :: HasVelocity b => p -> b -> b
slowDown dt a = a&velocity.xComponent %~ (*0.5)

-- | Only perform the action if the key is pressed
ifPressed   :: (a -> a) -> KeyState -> (a -> a)
ifPressed f = ifPressed' f id

ifPressed'     :: (a -> a) -> (a -> a) -> KeyState -> (a -> a)
ifPressed' f g = \case
                   Pressed  -> f
                   Released -> g

jump    :: Aladdin -> Aladdin
jump a' = if canJump (a'^.movementState) then j a' else a'
  where
    j a = a&velocity.yComponent %~ (\vy -> if vy == 0 then jumpVelocity else vy)
           &movementState       .~ Jumping

xDelta = 1

moveLeft  :: Aladdin -> Aladdin
moveLeft a = a&velocity.xComponent .~ (-1)*xDelta

moveRight :: Aladdin -> Aladdin
moveRight a = a&velocity.xComponent .~ xDelta

crouch   :: Aladdin -> Aladdin
crouch a = a&movementState %~ f
  where
    f = \case
          Standing -> Crouching
          s        -> s

unCrouch   :: Aladdin -> Aladdin
unCrouch a = a&movementState %~ f
  where
    f = \case
          Crouching -> Standing
          s         -> s


canCrouch   :: MovementState -> Bool
canCrouch s = s `elem` [Standing]

canJump   :: MovementState -> Bool
canJump s = s `elem` [Standing, Crouching, Climbing]

jumpVelocity :: R
jumpVelocity = 6


drawSword a = a&sword .~ Extended

shieldSword a = a&sword .~ Shielded

--------------------------------------------------------------------------------
-- * Moving

move           :: Level -> KeysState -> Time -> Aladdin -> Aladdin
move lvl ks dt = aladdinPhysics  dt ground (lvl^.bBox)
               . applyKeysState  dt ks
               . aladdinGravity  dt ground
  where
    ground = groundAt' lvl


-- -- | If we keep pressing the left/right keys we want to maintain the movement speed.
-- continuousInput      :: Double -> KeysState -> Aladdin -> Aladdin
-- continuousInput dt ks = f (ks^.leftKey) moveLeft . f (ks^.rightKey) moveRight
--   where
--     f k g = case k of
--               Pressed  -> g
--               Released -> \a -> a&velocity.xComponent %~ (*0.5)
--   -- TODO: just a test



  -- a&velocity.xComponent %~ ( ifPressed (+xDelta)         (ks^.rightKey)
  --                                               . ifPressed (subtract xDelta) (ks^.leftKey)
  --                                               )


--------------------------------------------------------------------------------

aladdinGravity  :: Double -> (Point 2 R -> R) -> Aladdin -> Aladdin
aladdinGravity dt ground a
    | a^.position.yCoord > ground (a^.position) = (applyGravity dt a)&movementState .~ Falling
    | otherwise                                 = a

-- | Compute gravity on an object
gravity             :: (HasPosition t, HasVelocity t)
                    => Double
                    -> (Point 2 R -> R) -- ^ find the ground level where we stop falling
                    -> t -> t
gravity dt ground a
    | a^.position.yCoord > ground (a^.position) = applyGravity dt a
    | otherwise                                 = a

-- | Actually apply the gravity function
applyGravity      :: HasVelocity t => Double -> t -> t
applyGravity dt a = a&velocity.yComponent %~ f
  where
    f vy = accell vy - (dt/4)

    accell = \case
      0  -> fallingSpeed
      vy -> vy

-- | Speed with which we start falling
fallingSpeed :: R
fallingSpeed = -6


-- | version specfic to aladdin
aladdinPhysics             :: Double -> (Point 2 R -> R)
                           -> Rectangle p R -> Aladdin -> Aladdin
aladdinPhysics dt ground r = physicsY . physicsX
  where
    physicsX a = a&position.xCoord +~ dt*a^.velocity.xComponent

    physicsY a | newY <= gp = a&movementState   %~ stopFalling
                               &position.yCoord .~ gp
               | otherwise  = a&position.yCoord .~ newY
      where
        newY = a^.position.yCoord + dt*a^.velocity.yComponent
        gp   = ground $ a^.position

-- | Perform the actual movement
physics             :: (HasPosition t, HasVelocity t)
                    => Double
                    -> (Point 2 R -> R) -- ^ ground level
                    -> t -> t
physics dt ground = physicsY . physicsX -- first do x, then do y, this prevents falling through
  where
    physicsX a = a&position.xCoord +~ dt*a^.velocity.xComponent
    physicsY a = a&position.yCoord %~ \y -> (y + dt*a^.velocity.yComponent)
                                            `max`
                                            ground (a^.position)

-- | Figure out if we should stop falling
stopFalling :: MovementState -> MovementState
stopFalling = \case
    Jumping -> Standing
    Falling -> Standing
    s       -> s





-- updateAladdin      ::  KeysState -> Aladdin -> Aladdin
-- updateAladdin ks a = F.foldl' (\a' f -> f a') a  update'
--   where
--     update' :: GKeysState (Aladdin -> Aladdin)
--     update' = update'' <*> ks
--     -- TODO: jump slash etc.

--     update'' :: GKeysState (KeyState -> Aladdin -> Aladdin)
--     update'' = KeysState  (ifPressed moveLeft)
--                           (ifPressed moveRight)
--                           (const id) -- up
--                           crouch
--                           (ifPressed jump)
--                           (const id) -- throw
--                           (const id) -- slash

--     crouch = \case
--                Pressed  -> (&movementState .~ Crouching)
--                Released -> (&movementState .~ Standing)


--     jump = case ks^.downKey of
--              Pressed  -> id -- cannot jump if we are also pressing key down
--              Released -> (\a' -> a'&movementState   .~ Jumping
--                                    &position.yCoord +~ yDelta
--                          )


-- yDelta = 5

-- -- if we are looking in the right direction yet; move in that direction
-- moveLeft  :: HasVelocity t => t -> t
-- moveLeft a = a&velocity.xComponent %~ \x -> if x < 0 then -xDelta

--   | a^.velocity.xComponent <= 0 = a&position.xCoord     -~ xDelta
--            | otherwise                   = a&velocity.xComponent .~ 0

-- moveRight   :: Aladdin -> Aladdin
-- moveRight a | a^.velocity.xComponent >= 0 = a&position.xCoord     +~ xDelta
--             | otherwise                   = a&velocity.xComponent .~ 0

--------------------------------------------------------------------------------
-- * Geometry stuff

groundAt'       :: Level -> Point 2 R -> R
groundAt' lvl p = case groundAt lvl p of
                    Bottom     -> lvl^.bBox.to minPoint.core.yCoord
                    ValB (y,_) -> y

-- | Find the topmost item below p
groundAt       :: Level -> Point 2 R -> Bottom (R,Item)
groundAt lvl p = shootRay p $ lvl^.obstacles

-- | Find the topmost item below p
shootRay   :: Point 2 R -> [Item] -> Bottom (R,Item)
shootRay p = maximumOn fst . concatMap isVal . map (shootRay' p)
  where
    isVal = \case
      Bottom -> []
      ValB x -> [x]

maximumOn   :: Ord b => (a -> b) -> [a] -> Bottom a
maximumOn f = \case
  [] -> Bottom
  xs -> ValB $ F.maximumBy (comparing f) xs


shootRay'     :: Point 2 R -> Item -> Bottom (R,Item)
shootRay' p i = (,i) <$> (f $ i^.itemBox)
  where
    f b | (p^.xCoord) `inRange` (b^.to extent.xComponent) = maxBelow (p^.yCoord) b
        | otherwise                                       = Bottom

maxBelow     :: R -> Rectangle p R -> Bottom R
maxBelow y b = let Range' l u = b^.to extent.yComponent
               in if y >= l then ValB $ y `min` u
                            else Bottom
