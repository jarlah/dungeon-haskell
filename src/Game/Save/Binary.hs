{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Orphan 'Binary' instances for every game type reachable from
--   'GameState'. Centralised here so 'Game.Types', 'Game.Core',
--   'Game.Logic.*' stay free of serialization concerns.
--
--   Import this module (or 'Game.Save', which re-exports it) whenever
--   you need to encode/decode game values.
module Game.Save.Binary () where

import           Data.Binary                (Binary (..))
import           Data.Vector.Binary         ()
import           GHC.Generics               (Generic)
import           System.Random.Internal     (StdGen (..))
import           System.Random.SplitMix     (seedSMGen, unseedSMGen)

import           Game.Core
import           Game.Logic.Chest           (Chest (..), ChestState (..))
import           Game.Logic.Quest
import           Game.Types

--------------------------------------------------------------------
-- StdGen
--------------------------------------------------------------------

-- Binary for StdGen: 'random' 1.2+ models 'StdGen' as an opaque
-- newtype around a splitmix 'SMGen', with no 'Read' instance, so
-- a naive Show/Read round-trip doesn't compile. Instead we reach
-- into 'System.Random.Internal' for the newtype constructor and
-- serialize the underlying splitmix state as two 'Word64's via
-- 'unseedSMGen' / 'seedSMGen'. This is 16 bytes, deterministic,
-- and preserves the exact RNG stream so a save/load round-trip
-- is bit-identical with a run that never saved.
instance Binary StdGen where
  put (StdGen smg) =
    let (a, b) = unseedSMGen smg
    in put a >> put b
  get = do
    a <- get
    StdGen . seedSMGen a <$> get

--------------------------------------------------------------------
-- Generic + Binary for every game type reachable from GameState.
--
-- StandaloneDeriving lets us keep Game.Types / Game.Core /
-- Game.Logic.Quest / Game.Logic.Dungeon completely free of
-- serialization concerns — every type is stock-derivable because
-- its constructors are exported.
--------------------------------------------------------------------

deriving instance Generic Dir
deriving instance Generic KeyId
deriving instance Generic DoorState
deriving instance Generic Tile
deriving instance Generic DungeonLevel
deriving instance Generic Stats
deriving instance Generic MonsterKind
deriving instance Generic Monster
deriving instance Generic GameEvent
deriving instance Generic Potion
deriving instance Generic Weapon
deriving instance Generic Armor
deriving instance Generic Item
deriving instance Generic Inventory
deriving instance Generic InventoryError
deriving instance Generic GameAction

deriving instance Generic Room

deriving instance Generic QuestGoal
deriving instance Generic QuestStatus
deriving instance Generic QuestEvent
deriving instance Generic Quest

deriving instance Generic NPC
deriving instance Generic ChestState
deriving instance Generic Chest
deriving instance Generic ParkedLevel
deriving instance Generic SaveMenu
deriving instance Generic SaveMenuMode
deriving instance Generic SaveMenuEntry
deriving instance Generic LaunchMenu
deriving instance Generic LaunchOption
deriving instance Generic DirectionalAction
deriving instance Generic GameState

instance Binary Dir
instance Binary KeyId
instance Binary DoorState
instance Binary Tile
instance Binary DungeonLevel
instance Binary Stats
instance Binary MonsterKind
instance Binary Monster
instance Binary GameEvent
instance Binary Potion
instance Binary Weapon
instance Binary Armor
instance Binary Item
instance Binary Inventory
instance Binary InventoryError
instance Binary GameAction

instance Binary Room

instance Binary QuestGoal
instance Binary QuestStatus
instance Binary QuestEvent
instance Binary Quest

instance Binary NPC
instance Binary ChestState
instance Binary Chest
instance Binary ParkedLevel
instance Binary SaveMenu
instance Binary SaveMenuMode
instance Binary SaveMenuEntry
instance Binary LaunchMenu
instance Binary LaunchOption
instance Binary DirectionalAction
instance Binary GameState
