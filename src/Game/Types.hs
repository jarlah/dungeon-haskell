module Game.Types
  ( Pos
  , Dir(..)
  , dirToOffset
  , DoorState(..)
  , Tile(..)
  , DungeonLevel(..)
  , Room(..)
  , posInRoom
  , roomIndexAt
  , tileAt
  , isWalkable
  , GameAction(..)
  , Stats(..)
  , MonsterKind(..)
  , monsterStats
  , monsterGlyph
  , monsterName
  , monsterFootprint
  , isBoss
  , Monster(..)
  , mkMonster
  , monsterTiles
  , monsterOccupies
  , GameEvent(..)
  , Potion(..)
  , Weapon(..)
  , Armor(..)
  , Item(..)
  , itemGlyph
  , itemName
  , Inventory(..)
  , emptyInventory
  , InventoryError(..)
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

-- | A single dungeon floor: tiles stored row-major, plus the list
--   of rooms the generator carved out before stamping corridors.
--
--   @dlRooms@ is kept alongside the tile grid because once the
--   generator is done the room geometry is otherwise lost — the
--   tiles don't distinguish a room floor from a corridor floor, so
--   after-the-fact "what room is the player in?" becomes a flood
--   fill. Storing the room list is cheap (a few rectangles) and
--   lets downstream features — boss room detection, AI room
--   descriptions, per-room triggers — look it up in O(n).
data DungeonLevel = DungeonLevel
  { dlWidth  :: !Int
  , dlHeight :: !Int
  , dlTiles  :: !(Vector Tile)
  , dlDepth  :: !Int
  , dlRooms  :: ![Room]
  } deriving (Eq, Show)

-- | A rectangular room carved into the dungeon. Coordinates are the
--   /top-left/ corner of the interior, so the room occupies
--   @[rX, rX+rW-1] × [rY, rY+rH-1]@ inclusive.
--
--   Defined here (rather than only in "Game.Logic.Dungeon") so
--   "Game.Types" can refer to it from 'DungeonLevel' without a
--   module cycle. Game.Logic.Dungeon re-exports this type.
data Room = Room
  { rX :: !Int
  , rY :: !Int
  , rW :: !Int
  , rH :: !Int
  } deriving (Eq, Show)

-- | Does the given position lie inside this room? Uses inclusive
--   bounds matching 'Room's geometry.
posInRoom :: Pos -> Room -> Bool
posInRoom (V2 x y) r =
     x >= rX r && x < rX r + rW r
  && y >= rY r && y < rY r + rH r

-- | Index of the first room in the list that contains the given
--   position, or 'Nothing' if the position is in a corridor / wall /
--   other non-room tile. The index is stable across a level
--   (generator output order) so the AI layer can use it as a cache
--   key without worrying about reordering.
roomIndexAt :: [Room] -> Pos -> Maybe Int
roomIndexAt rooms p = go 0 rooms
  where
    go _ []     = Nothing
    go i (r:rs)
      | posInRoom p r = Just i
      | otherwise     = go (i + 1) rs

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
  | Pickup
    -- ^ pick up whatever item is on the player's current tile
  | UseItem !Int
    -- ^ apply the default action (quaff / equip) to the item at
    --   the given index in 'invItems'
  | GoDownStairs
    -- ^ descend when standing on 'StairsDown'
  | GoUpStairs
    -- ^ ascend when standing on 'StairsUp'
  | Quit
  deriving (Eq, Show)

-- | Combat / survival stats, shared by player and monsters.
--
--   'sLevel' and 'sXP' are only meaningful for the player. Monsters
--   always have level 1 / 0 XP; they never gain XP, but storing the
--   fields here keeps the record uniform.
data Stats = Stats
  { sHP      :: !Int
  , sMaxHP   :: !Int
  , sAttack  :: !Int
  , sDefense :: !Int
  , sSpeed   :: !Int
  , sLevel   :: !Int
  , sXP      :: !Int
  } deriving (Eq, Show)

-- | All the kinds of monster the game currently knows how to spawn.
--   Most kinds occupy a single tile; 'Dragon' is the first boss and
--   occupies a 2×2 footprint (see 'monsterFootprint').
data MonsterKind = Rat | Goblin | Orc | Dragon
  deriving (Eq, Show, Enum, Bounded)

-- | Baseline stats for a monster of the given kind. Bosses are
--   noticeably tougher than any regular monster — the Dragon is
--   roughly 4-5x the HP of the toughest regular kind.
monsterStats :: MonsterKind -> Stats
monsterStats Rat    = baseStats  5  2 0 3
monsterStats Goblin = baseStats 10  4 1 4
monsterStats Orc    = baseStats 18  6 3 5
monsterStats Dragon = baseStats 80 12 6 4

-- | Helper: a level-1, zero-XP stat block from hp/atk/def/speed.
baseStats :: Int -> Int -> Int -> Int -> Stats
baseStats hp atk dfn spd = Stats
  { sHP      = hp
  , sMaxHP   = hp
  , sAttack  = atk
  , sDefense = dfn
  , sSpeed   = spd
  , sLevel   = 1
  , sXP      = 0
  }

monsterGlyph :: MonsterKind -> Char
monsterGlyph Rat    = 'r'
monsterGlyph Goblin = 'g'
monsterGlyph Orc    = 'o'
monsterGlyph Dragon = 'D'

monsterName :: MonsterKind -> String
monsterName Rat    = "rat"
monsterName Goblin = "goblin"
monsterName Orc    = "orc"
monsterName Dragon = "dragon"

-- | Rectangular footprint (width, height) a monster of this kind
--   occupies. The top-left of the rectangle is the canonical
--   'mPos'; all other tiles in @[mPos .. mPos + footprint - 1]@
--   are also considered "this monster" for collision and attack
--   resolution. Most kinds are 1×1.
monsterFootprint :: MonsterKind -> Pos
monsterFootprint Dragon = V2 2 2
monsterFootprint _      = V2 1 1

-- | True when a monster kind is a boss. Bosses fire 'EvBossKilled'
--   on death, satisfy 'GoalKillBoss' quests, and (on the boss
--   floor) have their own music cue.
isBoss :: MonsterKind -> Bool
isBoss Dragon = True
isBoss _      = False

data Monster = Monster
  { mKind      :: !MonsterKind
  , mPos       :: !Pos
    -- ^ top-left corner of the monster's footprint
  , mStats     :: !Stats
  , mFootprint :: !Pos
    -- ^ cached @(width, height)@ of this monster, derived from
    --   its kind via 'monsterFootprint'. Kept on the record so
    --   footprint lookups don't chase through a case per query
    --   and so future bosses with per-instance footprints (e.g.
    --   a kraken that grows over time) can override it.
  } deriving (Eq, Show)

-- | Build a 'Monster' and stamp its footprint from its kind. Use
--   this instead of the raw record constructor so new kinds only
--   need a 'monsterFootprint' entry to Just Work.
mkMonster :: MonsterKind -> Pos -> Monster
mkMonster k p = Monster
  { mKind      = k
  , mPos       = p
  , mStats     = monsterStats k
  , mFootprint = monsterFootprint k
  }

-- | Every tile a monster occupies, top-left first. For 1×1
--   creatures this is just @[mPos m]@.
monsterTiles :: Monster -> [Pos]
monsterTiles m =
  let V2 w h = mFootprint m
      V2 x y = mPos m
  in [ V2 (x + dx) (y + dy) | dy <- [0 .. h - 1], dx <- [0 .. w - 1] ]

-- | Does this monster occupy the given tile? Used by attack and
--   movement resolution so a player swinging at any tile of a
--   boss's footprint lands on the boss.
monsterOccupies :: Monster -> Pos -> Bool
monsterOccupies m p = p `elem` monsterTiles m

-- | Semantic events produced by turn resolution. The pure logic layer
--   emits these as a side-product of applying an action; the IO shell
--   (audio, eventually particle effects, telemetry, ...) consumes them.
--
--   Keep the set small and meaningful — one event per "thing a player
--   would want feedback for".
data GameEvent
  = EvAttackMiss     -- ^ player swung and missed
  | EvAttackHit      -- ^ player landed a normal hit
  | EvAttackCrit     -- ^ player landed a critical hit
  | EvMonsterKilled  -- ^ player killed a monster
  | EvBossKilled     -- ^ player landed the killing blow on a boss
  | EvPlayerHurt     -- ^ a monster hit the player
  | EvPlayerDied     -- ^ a monster dealt the killing blow
  | EvLevelUp        -- ^ player gained an experience level
  | EvQuestTurnedIn  -- ^ a ready quest was handed in for reward
  deriving (Eq, Show)

------------------------------------------------------------
-- Items and inventory (M5)
------------------------------------------------------------

-- | Consumable healing potions.
data Potion
  = HealingMinor
  | HealingMajor
  deriving (Eq, Show, Enum, Bounded)

-- | Equippable melee weapons.
data Weapon
  = ShortSword
  | LongSword
  deriving (Eq, Show, Enum, Bounded)

-- | Equippable body armor.
data Armor
  = LeatherArmor
  | ChainMail
  deriving (Eq, Show, Enum, Bounded)

-- | An item that can appear on the floor or in a player's inventory.
data Item
  = IPotion !Potion
  | IWeapon !Weapon
  | IArmor  !Armor
  deriving (Eq, Show)

-- | ASCII glyph used to render an item on the dungeon floor or in
--   the inventory screen. Follows the rogue/NetHack convention:
--   @!@ potions, @)@ weapons, @[@ armor.
itemGlyph :: Item -> Char
itemGlyph (IPotion _) = '!'
itemGlyph (IWeapon _) = ')'
itemGlyph (IArmor  _) = '['

-- | Human-readable name for message log / inventory listing.
itemName :: Item -> String
itemName (IPotion HealingMinor) = "minor healing potion"
itemName (IPotion HealingMajor) = "major healing potion"
itemName (IWeapon ShortSword)   = "short sword"
itemName (IWeapon LongSword)    = "long sword"
itemName (IArmor  LeatherArmor) = "leather armor"
itemName (IArmor  ChainMail)    = "chain mail"

-- | What the player is carrying. 'invItems' holds unequipped items
--   in pickup order; 'invWeapon' / 'invArmor' are the currently
--   equipped slots. Equipping swaps the held piece for whatever is
--   currently equipped (the previous piece goes back into
--   'invItems'), so equip is never destructive.
data Inventory = Inventory
  { invItems  :: ![Item]
  , invWeapon :: !(Maybe Weapon)
  , invArmor  :: !(Maybe Armor)
  } deriving (Eq, Show)

-- | An empty inventory with nothing equipped.
emptyInventory :: Inventory
emptyInventory = Inventory
  { invItems  = []
  , invWeapon = Nothing
  , invArmor  = Nothing
  }

-- | Errors that can arise from inventory operations.
data InventoryError
  = InventoryFull
  deriving (Eq, Show)
