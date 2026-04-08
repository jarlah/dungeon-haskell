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

data MonsterKind = Rat | Goblin | Orc
  deriving (Eq, Show, Enum, Bounded)

-- | Baseline stats for a monster of the given kind.
monsterStats :: MonsterKind -> Stats
monsterStats Rat    = baseStats  5 2 0 3
monsterStats Goblin = baseStats 10 4 1 4
monsterStats Orc    = baseStats 18 6 3 5

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

monsterName :: MonsterKind -> String
monsterName Rat    = "rat"
monsterName Goblin = "goblin"
monsterName Orc    = "orc"

data Monster = Monster
  { mKind  :: !MonsterKind
  , mPos   :: !Pos
  , mStats :: !Stats
  } deriving (Eq, Show)

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
  | EvPlayerHurt     -- ^ a monster hit the player
  | EvPlayerDied     -- ^ a monster dealt the killing blow
  | EvLevelUp        -- ^ player gained an experience level
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
