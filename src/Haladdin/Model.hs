{-# LANGUAGE TemplateHaskell #-}
module Haladdin.Model where

import Data.Ext
import Control.Lens hiding (Level)
import Data.Geometry.Point
import Data.Geometry.Vector
import Data.Geometry.Box
import Data.List.NonEmpty as NonEmpty

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
-- * Player and Aladdin itself


newtype Health = Health Word deriving (Show,Eq,Ord)

newtype Score = Score Word deriving (Show,Eq,Ord)

data MovementState = Standing
                   | Crouching
                   | Jumping
                   | Climbing
                   deriving (Show,Eq)

-- | State of Aladdin's sword
data Sword = Shielded
           | Extended
           | Slashing
  deriving (Show,Eq)


-- | The type representing Aladdin; the character that the player controls
data Aladdin = Aladdin { _position      :: Point  2 R
                       , _velocity      :: Vector 2 R
                       , _movementState :: MovementState
                       , _apples        :: Count
                       , _sword         :: Sword
                       , _health        :: Health
                       } deriving (Show,Eq)
makeLenses ''Aladdin


newtype RespawnPoint = RespawnPoint { _respawnPoint :: Point 2 R }
                     deriving (Show,Eq,Ord)
makeLenses ''RespawnPoint


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
data Enemy = Enemy { _enemyPosition   :: Point 2 R
                   , _enemyKind       :: EnemyKind
                   , _enemyState      :: EnemyState
                   , _enemyHealth     :: Health
                   } deriving (Show,Eq)
makeLenses ''Enemy


--------------------------------------------------------------------------------
-- * The World


data ItemKind = Wall
              | Water
              | Coal
              | Ladder
              | SavePoint
              deriving (Show,Read,Eq)

-- | The world consists of "items"
data Item = Item { _obstacleBox  :: Rectangle () R
                 , _obstacleKind :: ItemKind
                 } deriving (Show,Eq)
makeLenses ''Item


data CollectableKind = CollectableApple
                     | CollectableRuby
                     deriving (Show,Eq)

-- | Alladin can collect things; these Collectable Items have a location and a kind.
data Collectable = Collectable { _collectableLocation :: Point 2 R
                               , _collectableKind     :: CollectableKind
                               } deriving (Show,Eq)
makeLenses ''Collectable

-- | The world consists of various levels. Levels consist of items,
-- collectables, enemies and have a target s.t. if aladdin arrives at the
-- target the level is complete.
data Level = Level { _collectables :: [Collectable]
                   , _obstacles    :: [Item]
                   , _enemies      :: [Enemy]
                   , _target       :: Rectangle () R -- ^ if the player reaches here
                                                     -- the level is complete
                   } deriving (Show,Eq)
makeLenses ''Level

-- | Data type representing the world. Esentially a zipper with Levels.  Note
-- that unfinishedLevels will contain an (unmodified) copy of the activeLevel
-- as well.
data World = World { _completedLevels  :: [Level]
                   , _activeLevel      :: Level
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
                           , _deltaTime :: Double
                           } deriving (Show,Eq)
makeLenses ''GameState


data GameMode = Playing !GameState
              | Paused !GameState
              | GameOver Score
              | Finished Score
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
