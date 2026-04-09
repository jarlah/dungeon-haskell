-- | Pure inventory operations: pickup, drop, equip, quaff, and
--   stat-effect helpers. Everything here is a plain 'Inventory' /
--   'Stats' transform — no RNG, no IO, no game-state dependency —
--   so it can be property-tested in isolation and reused from the
--   GameState layer without knot-tying.
module Game.Logic.Inventory
  ( -- * Capacity
    invCapacity
  , inventoryCount
  , isFull
    -- * Core operations
  , addItem
  , removeItem
  , dropItem
    -- * Equip / use
  , equip
  , quaffPotion
    -- * Combat stat helpers
  , weaponBonus
  , armorBonus
  , effectiveStats
    -- * Potion effects
  , potionHealAmount
  ) where

import Game.Types

-- | Maximum number of items the player can carry in the unequipped
--   bag. Equipped weapon/armor slots don't count against this.
--   Chosen small enough that the inventory letter list stays
--   one-page (a–z has 26 slots, so 20 leaves some headroom).
invCapacity :: Int
invCapacity = 20

-- | Number of items currently in the unequipped bag.
inventoryCount :: Inventory -> Int
inventoryCount = length . invItems

-- | True iff 'addItem' will refuse the next pickup.
isFull :: Inventory -> Bool
isFull inv = inventoryCount inv >= invCapacity

-- | Append an item to the bag. Fails with 'InventoryFull' at the
--   capacity limit. Equipped slots are untouched.
addItem :: Item -> Inventory -> Either InventoryError Inventory
addItem item inv
  | isFull inv = Left InventoryFull
  | otherwise  = Right inv { invItems = invItems inv ++ [item] }

-- | Take the item at @i@ out of the bag. 'Nothing' if the index is
--   out of range.
removeItem :: Int -> Inventory -> Maybe (Item, Inventory)
removeItem i inv
  | i < 0 || i >= inventoryCount inv = Nothing
  | otherwise =
      let xs           = invItems inv
          (before, rest) = splitAt i xs
      in case rest of
           []        -> Nothing
           (x : xs') -> Just (x, inv { invItems = before ++ xs' })

-- | Convenience: drop-by-index, discarding the result of
--   'removeItem' if the index is out of range. Useful for tests
--   where we just want the updated inventory.
dropItem :: Int -> Inventory -> Inventory
dropItem i inv = maybe inv snd (removeItem i inv)

-- | Equip an item taken out of the bag. For weapons and armor, the
--   previously-equipped piece (if any) is pushed back into the bag;
--   for potions this is a no-op (they are consumed by 'quaffPotion',
--   not equipped).
--
--   The item at the given index is REMOVED from the bag before the
--   swap happens, so there is never a moment where the player is
--   holding two of the same slot.
equip :: Int -> Inventory -> Inventory
equip i inv = case removeItem i inv of
  Nothing            -> inv
  Just (item, inv') -> case item of
    IWeapon w ->
      let inv'' = inv' { invWeapon = Just w }
      in case invWeapon inv of
           Nothing   -> inv''
           Just prev -> inv'' { invItems = invItems inv'' ++ [IWeapon prev] }
    IArmor a ->
      let inv'' = inv' { invArmor = Just a }
      in case invArmor inv of
           Nothing   -> inv''
           Just prev -> inv'' { invItems = invItems inv'' ++ [IArmor prev] }
    IPotion _ ->
      -- Potions aren't equipped; put it back where it was. Callers
      -- should route potions through 'quaffPotion' instead.
      inv
    IKey _ ->
      -- Keys aren't equipped; they're consumed by bumping the
      -- matching locked door. Leave the bag untouched.
      inv

-- | Quaff a potion: consume it (caller removes from bag) and apply
--   its effect to the supplied stats. The returned stats never
--   exceed 'sMaxHP'.
quaffPotion :: Potion -> Stats -> Stats
quaffPotion p s =
  let healed = sHP s + potionHealAmount p
  in s { sHP = min healed (sMaxHP s) }

-- | Raw HP restored by a potion, before the max-HP clamp.
potionHealAmount :: Potion -> Int
potionHealAmount HealingMinor = 5
potionHealAmount HealingMajor = 15

-- | Attack bonus granted by an equipped weapon.
weaponBonus :: Weapon -> Int
weaponBonus ShortSword = 2
weaponBonus LongSword  = 4

-- | Defense bonus granted by equipped armor.
armorBonus :: Armor -> Int
armorBonus LeatherArmor = 1
armorBonus ChainMail    = 3

-- | Combat stat block that layers equipped gear on top of the
--   base stats. Combat code should always call this before
--   resolving attacks so weapons and armor matter.
effectiveStats :: Stats -> Inventory -> Stats
effectiveStats s inv = s
  { sAttack  = sAttack  s + maybe 0 weaponBonus (invWeapon inv)
  , sDefense = sDefense s + maybe 0 armorBonus  (invArmor  inv)
  }
