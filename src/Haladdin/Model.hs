module Haladdin.Model where

import Control.Lens hiding (Level)
import Data.Geometry.Point
import Data.Geometry.Vector
import Data.Geometry.Box


--------------------------------------------------------------------------------
-- * General Types

type Model = GameMode


type R = Double

type Count = Word

data GameMode = Playing GameState
              | Paused GameState
              | GameOver
              | Finished
              deriving (Show,Eq)

-- | The main Gamestate
data GameState = GameState { _world    :: World
                           , _player   :: Player
                           , _viewport :: ViewPort
                           } deriving (Show,Eq)


data ViewPort = ViewPort { _viewPortCenter     :: Point 2 R
                         , _viewPortDimensions :: Vector 2 R
                         } deriving (Show,Eq)

--------------------------------------------------------------------------------
-- * The World

data World = World { _prevLevels       :: [Level]
                   , _activeLevel      :: Level
                   , _unfinishedLevels :: [Level]
                   } deriving (Show,Eq)


data Level = Level { _collectables :: [Collectable]
                   , _obstacles    :: [Item]
                   , _enemies      :: [Enemy]
                   , _target       :: Rectangle () R -- ^ if the player reaches here
                                                     -- the level is complete
                   } deriving (Show,Eq)


data Collectable = Collectable { _collectableLocation :: Point 2 R
                               , _collectableKind     :: CollectableKind
                               } deriving (Show,Eq)

data CollectableKind = CollectableApple
                     | CollectableRuby
                     deriving (Show,Eq)


data Item = Item { _obstacleBox  :: Rectangle () R
                 , _obstacleKind :: ItemKind
                 } deriving (Show,Eq)

data ItemKind = Wall
              | Water
              | Coal
              | Ladder
              | SavePoint
              deriving (Show,Read,Eq)


--------------------------------------------------------------------------------
-- * Player and Aladdin itself


newtype Health = Health Word deriving (Show,Eq,Ord)

newtype Score = Score Word deriving (Show,Eq,Ord)


data Player = Player { _aladdin         :: Aladdin
                     , _lives           :: Count
                     , _rubies          :: Count
                     , _score           :: Score
                     , _respawnLocation :: RespawnPoint
                     } deriving (Show,Eq)

newtype RespawnPoint = RespawnPoint { _respawnPoint :: Point 2 R }
                     deriving (Show,Eq,Ord)

data Aladdin = Aladdin { _position      :: Point 2 R
                       , _movementState :: MovementState
                       , _apples        :: Count
                       , _sword         :: Sword
                       , _health        :: Health
                       } deriving (Show,Eq)

data MovementState = Standing
                   | Crouching
                   | Jumping
                   | Climbing
                   deriving (Show,Eq)


-- | State of the sword
data Sword = Shielded
           | Extended
           | Slashing
  deriving (Show,Eq)


--------------------------------------------------------------------------------
-- * Enemies

data Enemy = Enemy { _enemyPosition   :: Point 2 R
                   , _enemyKind       :: EnemyKind
                   , _enemyState      :: EnemyState
                   , _enemyHealth     :: Health
                   } deriving (Show,Eq)

data EnemyState = Patrolling
                | Taunting
                | Attacking
                deriving (Show,Eq)
-- maybe refine this depending on the enemyKind


data EnemyKind = Marketman
               | Soldier
               | Snake
               deriving (Show,Eq)
