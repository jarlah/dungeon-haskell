module Game.Types
  ( Pos
  , Dir(..)
  , dirToOffset
  , DoorState(..)
  , Tile(..)
  , DungeonLevel(..)
  , tileAt
  , isWalkable
  , GameAction(..)
  , Stats(..)
  , MonsterKind(..)
  , monsterStats
  , monsterGlyph
  , monsterName
  , Monster(..)
  ) where

import Data.Vector (Vector, (!))
import Linear (V2(..))

-- | Grid position (column, row).
type Pos = V2 Int

-- | Cardinal + diagonal directions.
data Dir = N | NE | E | SE | S | SW | W | NW
  deriving (Eq, Show, Enum, Bounded)

dirToOffset :: Dir -> Pos
dirToOffset N  = V2 0 (-1)
dirToOffset NE = V2 1 (-1)
dirToOffset E  = V2 1 0
dirToOffset SE = V2 1 1
dirToOffset S  = V2 0 1
dirToOffset SW = V2 (-1) 1
dirToOffset W  = V2 (-1) 0
dirToOffset NW = V2 (-1) (-1)

data DoorState = Open | Closed
  deriving (Eq, Show)

data Tile = Floor | Wall | Door DoorState | StairsDown | StairsUp
  deriving (Eq, Show)

-- | A single dungeon floor: tiles stored row-major.
data DungeonLevel = DungeonLevel
  { dlWidth  :: !Int
  , dlHeight :: !Int
  , dlTiles  :: !(Vector Tile)
  , dlDepth  :: !Int
  } deriving (Eq, Show)

-- | Look up the tile at a position, or 'Nothing' if out of bounds.
tileAt :: DungeonLevel -> Pos -> Maybe Tile
tileAt dl (V2 x y)
  | x < 0 || y < 0 || x >= dlWidth dl || y >= dlHeight dl = Nothing
  | otherwise = Just $ dlTiles dl ! (y * dlWidth dl + x)

isWalkable :: Tile -> Bool
isWalkable Floor         = True
isWalkable (Door Open)   = True
isWalkable StairsDown    = True
isWalkable StairsUp      = True
isWalkable Wall          = False
isWalkable (Door Closed) = False

-- | Actions the player (or AI) can attempt on a turn.
data GameAction
  = Move Dir
  | Wait
  | Quit
  deriving (Eq, Show)

-- | Combat / survival stats, shared by player and monsters.
data Stats = Stats
  { sHP      :: !Int
  , sMaxHP   :: !Int
  , sAttack  :: !Int
  , sDefense :: !Int
  , sSpeed   :: !Int
  } deriving (Eq, Show)

data MonsterKind = Rat | Goblin | Orc
  deriving (Eq, Show, Enum, Bounded)

-- | Baseline stats for a monster of the given kind.
monsterStats :: MonsterKind -> Stats
monsterStats Rat    = Stats  5  5 2 0 3
monsterStats Goblin = Stats 10 10 4 1 4
monsterStats Orc    = Stats 18 18 6 3 5

monsterGlyph :: MonsterKind -> Char
monsterGlyph Rat    = 'r'
monsterGlyph Goblin = 'g'
monsterGlyph Orc    = 'o'

monsterName :: MonsterKind -> String
monsterName Rat    = "rat"
monsterName Goblin = "goblin"
monsterName Orc    = "orc"

data Monster = Monster
  { mKind  :: !MonsterKind
  , mPos   :: !Pos
  , mStats :: !Stats
  } deriving (Eq, Show)
