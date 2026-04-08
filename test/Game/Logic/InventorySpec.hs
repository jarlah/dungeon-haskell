{-# LANGUAGE ScopedTypeVariables #-}
-- | Property + example tests for 'Game.Logic.Inventory'.
module Game.Logic.InventorySpec (spec) where

import Data.Maybe (fromJust)
import Test.Hspec
import Test.QuickCheck

import Game.Logic.Inventory
import Game.Types

-- | QuickCheck generators.

instance Arbitrary Potion where
  arbitrary = elements [HealingMinor, HealingMajor]

instance Arbitrary Weapon where
  arbitrary = elements [ShortSword, LongSword]

instance Arbitrary Armor where
  arbitrary = elements [LeatherArmor, ChainMail]

instance Arbitrary Item where
  arbitrary = oneof
    [ IPotion <$> arbitrary
    , IWeapon <$> arbitrary
    , IArmor  <$> arbitrary
    ]

-- | A stat block valid for potion tests: positive HP/maxHP, HP
--   never exceeding maxHP.
genStats :: Gen Stats
genStats = do
  maxHP <- choose (1, 100)
  hp    <- choose (0, maxHP)
  pure Stats
    { sHP      = hp
    , sMaxHP   = maxHP
    , sAttack  = 5
    , sDefense = 2
    , sSpeed   = 10
    , sLevel   = 1
    , sXP      = 0
    }

spec :: Spec
spec = describe "Game.Logic.Inventory" $ do

  describe "addItem / removeItem" $ do

    it "empty inventory has count zero" $
      inventoryCount emptyInventory `shouldBe` 0

    it "adding then removing by index yields the same item" $ property $ \(it :: Item) ->
      case addItem it emptyInventory of
        Left  _    -> property False
        Right inv' -> case removeItem 0 inv' of
          Just (back, _) -> back === it
          Nothing        -> property False

    it "pickUpThenDrop returns the item and restores the inventory" $ property $ \(it :: Item) ->
      let Right inv' = addItem it emptyInventory
          (back, restored) = fromJust (removeItem 0 inv')
      in back === it .&&. restored === emptyInventory

    it "cannot exceed capacity" $ property $ \(its :: [Item]) ->
      let attempts = take (invCapacity + 5) its
          result   = foldr (\x acc -> acc >>= addItem x) (Right emptyInventory) attempts
      in case result of
           Right inv' -> inventoryCount inv' <= invCapacity
           Left  InventoryFull -> length attempts > invCapacity

    it "inventory count is never negative" $ property $ \(its :: [Item]) ->
      let result = foldr (\x acc -> acc >>= addItem x) (Right emptyInventory) its
      in case result of
           Right inv' -> inventoryCount inv' >= 0
           Left  _    -> True

  describe "quaffPotion" $ do

    it "never exceeds sMaxHP" $ property $ forAll genStats $ \s ->
      forAll (arbitrary :: Gen Potion) $ \p ->
        let s' = quaffPotion p s
        in sHP s' <= sMaxHP s'

    it "never decreases HP" $ property $ forAll genStats $ \s ->
      forAll (arbitrary :: Gen Potion) $ \p ->
        sHP (quaffPotion p s) >= sHP s

    it "a minor potion on a full-HP player is a no-op" $ do
      let s = Stats { sHP = 20, sMaxHP = 20, sAttack = 5
                   , sDefense = 2, sSpeed = 10, sLevel = 1, sXP = 0 }
      sHP (quaffPotion HealingMinor s) `shouldBe` 20

    it "a major potion heals more than a minor one when there's room" $ property $
      forAll (choose (1, 50)) $ \maxHP ->
        let s = Stats { sHP = 0, sMaxHP = maxHP + 20, sAttack = 5
                     , sDefense = 2, sSpeed = 10, sLevel = 1, sXP = 0 }
        in sHP (quaffPotion HealingMajor s) > sHP (quaffPotion HealingMinor s)

  describe "equip" $ do

    it "equipping a weapon from the bag moves it to the weapon slot" $ do
      let Right inv = addItem (IWeapon ShortSword) emptyInventory
          inv'      = equip 0 inv
      invWeapon inv' `shouldBe` Just ShortSword
      invItems  inv' `shouldBe` []

    it "equipping a second weapon swaps the old one back into the bag" $ do
      let Right inv1 = addItem (IWeapon ShortSword) emptyInventory
          inv2       = equip 0 inv1            -- ShortSword equipped
          Right inv3 = addItem (IWeapon LongSword) inv2
          inv4       = equip 0 inv3            -- LongSword equipped, ShortSword back
      invWeapon inv4 `shouldBe` Just LongSword
      invItems  inv4 `shouldBe` [IWeapon ShortSword]

    it "equipping an armor from the bag moves it to the armor slot" $ do
      let Right inv = addItem (IArmor LeatherArmor) emptyInventory
          inv'      = equip 0 inv
      invArmor inv' `shouldBe` Just LeatherArmor

  describe "effectiveStats" $ do

    it "equipping a weapon increases attack" $ property $ forAll genStats $ \s ->
      let Right inv1 = addItem (IWeapon ShortSword) emptyInventory
          inv2       = equip 0 inv1
      in sAttack (effectiveStats s inv2) > sAttack s

    it "equipping armor increases defense" $ property $ forAll genStats $ \s ->
      let Right inv1 = addItem (IArmor ChainMail) emptyInventory
          inv2       = equip 0 inv1
      in sDefense (effectiveStats s inv2) > sDefense s

    it "with nothing equipped, stats pass through unchanged" $ property $ forAll genStats $ \s ->
      effectiveStats s emptyInventory === s
