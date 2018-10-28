{-# LANGUAGE TemplateHaskell #-}
module Haladdin.Model where

import           Control.Lens hiding (Level)
import           Data.Ext
import           Data.Geometry.Box
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Vector
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))

--------------------------------------------------------------------------------
-- * General Types



type R = Double

type Count = Word


-- | The viewport
data ViewPort = ViewPort { _viewPortCenter     :: Point 2 R
                         , _viewPortDimensions :: Vector 2 R
                         } deriving (Show,Eq)
makeLenses ''ViewPort


-- | A key can be released or pressed
data KeyState = Released | Pressed deriving (Show,Read,Eq,Enum,Bounded)

toggleKey          :: KeyState -> KeyState
toggleKey Released = Pressed
toggleKey Pressed  = Released

-- | Status of all keys
data GKeysState a = KeysState { _leftKey  :: a
                              , _rightKey :: a
                              , _downKey  :: a
                              , _upKey    :: a
                              , _jumpKey  :: a
                              , _throwKey :: a
                              , _slashKey :: a
                              }
                 deriving (Show,Eq,Functor,Foldable,Traversable)
makeLenses ''GKeysState

instance Applicative GKeysState where
  pure x = KeysState x x x x x x x
  (KeysState l r d u j t s) <*> (KeysState lx rx dx ux jx tx sx) =
    KeysState (l lx) (r rx) (d dx) (u ux) (j jx) (t tx) (s sx)

type KeysState = GKeysState KeyState

allKeys :: a -> GKeysState a
allKeys = pure


--------------------------------------------------------------------------------
-- * Some genereric typeclasses

class HasPosition t where
  position :: Lens' t (Point 2 R)

class HasVelocity t where
  velocity :: Lens' t (Vector 2 R)

class HasHealth t where
  health :: Lens' t Health

--------------------------------------------------------------------------------
-- * Player and Aladdin itself


newtype Health = Health Word deriving (Show,Eq,Ord)

newtype Score = Score Word deriving (Show,Eq,Ord)

data MovementState = Standing
                   | Crouching
                   | Jumping
                   | Falling
                   | Climbing
                   deriving (Show,Eq)

-- | State of Aladdin's sword
data Sword = Shielded
           | Extended
           | Slashing
  deriving (Show,Eq)


-- | Throwable "weapons", i.e. apples, swords, etc.
data Throwable = Throwable { _throwableCenter   :: Point  2 R
                           , _throwableVelocity :: Vector 2 R
                           , _throwableSize     :: Vector 2 R
                           } deriving (Show,Eq)
makeLenses ''Throwable

instance HasPosition Throwable where position = throwableCenter
instance HasVelocity Throwable where velocity = throwableVelocity

class AsThrowable t where
  throwable :: Lens' t Throwable

newtype Apple = Apple Throwable deriving (Show,Eq)

instance AsThrowable Apple where throwable = lens (\(Apple t) -> t) (const Apple)
instance HasPosition Apple where position = throwable.position
instance HasVelocity Apple where velocity = throwable.velocity


-- | The type representing Aladdin; the character that the player controls
data Aladdin = Aladdin { _aladdinPosition :: Point  2 R
                       , _aladdinVelocity :: Vector 2 R
                       , _movementState   :: MovementState
                       , _apples          :: Count
                       , _sword           :: Sword
                       , _aladdinHealth   :: Health
                       } deriving (Show,Eq)
makeLenses ''Aladdin

instance HasPosition Aladdin where position = aladdinPosition
instance HasVelocity Aladdin where velocity = aladdinVelocity
instance HasHealth   Aladdin where health   = aladdinHealth



newtype RespawnPoint = RespawnPoint { _respawnPoint :: Point 2 R }
                     deriving (Show,Eq,Ord)
makeLenses ''RespawnPoint


instance HasPosition RespawnPoint where position = respawnPoint


-- | Type representing the player
data Player = Player { _aladdin         :: Aladdin
                     , _lives           :: Count
                     , _rubies          :: Count
                     , _score           :: Score
                     , _respawnLocation :: RespawnPoint
                     } deriving (Show,Eq)
makeLenses ''Player


--------------------------------------------------------------------------------
-- * Enemies

-- this needds some redefinitions I think; I think I want to use Vinyl's CoRec again.

data EnemyState = Patrolling
                | Taunting
                | Attacking
                deriving (Show,Eq)
-- maybe refine this depending on the enemyKind

data EnemyKind = Marketman
               | Soldier
               | Snake
               deriving (Show,Eq)

-- | Data type representing Alladin's enemies
data Enemy = Enemy { _enemyPosition   :: Point  2 R
                   , _enemyVelocity   :: Vector 2 R
                   , _enemyKind       :: EnemyKind
                   , _enemyState      :: EnemyState
                   , _enemyHealth     :: Health
                   } deriving (Show,Eq)
makeLenses ''Enemy

instance HasPosition Enemy where position = enemyPosition
instance HasVelocity Enemy where velocity = enemyVelocity
instance HasHealth   Enemy where health   = enemyHealth

type instance Dimension Enemy = 2
type instance NumType Enemy = R

instance IsBoxable Enemy where
  boundingBox e = boundingBox $ e^.position

--------------------------------------------------------------------------------
-- * The World


data ItemKind = Wall
              | Water
              | Coal
              | Ladder
              deriving (Show,Read,Eq)

-- | The world consists of "items"
data Item = Item { _itemBox  :: !(Rectangle () R)
                 , _itemKind :: !ItemKind
                 } deriving (Show,Eq)
makeLenses ''Item

type instance Dimension Item = 2
type instance NumType Item = R

instance IsBoxable Item where
  boundingBox i = i^.itemBox

data CollectableKind = CollectableApple
                     | CollectableRuby
                     | SavePoint
                     deriving (Show,Eq)

-- | Alladin can collect things; these Collectable Items have a location and a kind.
data Collectable = Collectable { _collectablePosition :: !(Point 2 R)
                               , _collectableKind     :: !CollectableKind
                               } deriving (Show,Eq)
makeLenses ''Collectable

instance HasPosition Collectable where position = collectablePosition

type instance Dimension Collectable = 2
type instance NumType Collectable = R

instance IsBoxable Collectable where
  boundingBox c = boundingBox $ c^.position



type TargetArea = Rectangle () R

-- | The world consists of various levels. Levels consist of items,
-- collectables, enemies and have a target s.t. if aladdin arrives at the
-- target the level is complete.
data Level = Level { _collectables :: [Collectable]
                   , _obstacles    :: [Item]
                   , _enemies      :: [Enemy]
                   , _target       :: !TargetArea -- ^ if the player reaches here
                                                     -- the level is complete
                   , _bBox         :: Rectangle () R
                   } deriving (Show,Eq)
makeLenses ''Level

type instance Dimension Level = 2
type instance NumType Level = R

-- | Smart constructor for making a level
level            :: [Collectable] -> [Item] -> [Enemy] -> TargetArea -> Level
level cs is es t = Level cs is es t bb
  where
    bb = boundingBoxList $ t :| concat [ map boundingBox cs
                                       , map boundingBox is
                                       , map boundingBox es
                                       ]

instance IsBoxable Level where
  boundingBox l = l^.bBox


-- | Data type representing the world. Esentially a zipper with Levels.  Note
-- that unfinishedLevels will contain an (unmodified) copy of the activeLevel
-- as well.
data World = World { _completedLevels  :: [Level]
                   , _activeLevel      :: !Level
                   , _unfinishedLevels :: NonEmpty Level
                   } deriving (Show,Eq)
makeLenses ''World

--------------------------------------------------------------------------------
-- * GameState

-- | The main Gamestate
data GameState = GameState { _world     :: !World
                           , _player    :: !Player
                           , _viewPort  :: !ViewPort
                           , _keysState :: !KeysState
                           , _deltaTime :: !Double
                           } deriving (Show,Eq)
makeLenses ''GameState


data GameMode = Playing !GameState
              | Paused !GameState
              | GameOver !Score
              | Finished !Score
              deriving (Show,Eq)
makePrisms ''GameMode



type Model = GameMode


--------------------------------------------------------------------------------
-- * Initial Values

initialModel :: GameMode
initialModel = Playing initialGameState

initialGameState :: GameState
initialGameState = GameState initialWorld initialPlayer initialViewPort initialKeysState 0

initialWorld :: World
initialWorld = World [] (NonEmpty.head allLevels) allLevels

allLevels :: NonEmpty Level
allLevels = NonEmpty.fromList
    [ level []
            [ Item (box (ext $ Point2 0 (-1000))
                        (ext $ Point2 1300 0))
                   Wall
            , Item (box (ext $ Point2 1500 (-1000))
                        (ext $ Point2 2000 0))
                   Wall
            , Item (box (ext $ Point2 300 0)
                        (ext $ Point2 800 200))
                   Wall
            , Item (box (ext $ Point2 1000 0)
                        (ext $ Point2 1200 10000))
                   Wall
            ] []
            $ box (ext $ Point2 10 10) (ext $ Point2 11 11)
    ]

initialPlayer :: Player
initialPlayer =
    Player initialAladin initialLives 0 (Score 0) initialRespawnPoint

initialRespawnPoint :: RespawnPoint
initialRespawnPoint = RespawnPoint origin

initialViewPort :: ViewPort
initialViewPort = let w = 800
                      h = 600
                  in ViewPort (Point2 (w/2) (h*0.4)) (Vector2 w h)

initialKeysState :: KeysState
initialKeysState = allKeys Released

initialAladin :: Aladdin
initialAladin = Aladdin (initialRespawnPoint^.respawnPoint)
                        (Vector2 0 0)
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
