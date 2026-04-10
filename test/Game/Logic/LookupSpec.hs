module Game.Logic.LookupSpec (spec) where

import Test.Hspec
import Linear (V2(..))

import Game.Types
  ( Pos, Monster(..), MonsterKind(..), Item(..), Potion(..)
  , KeyId(..), Inventory(..), mkMonster, emptyInventory
  )
import Game.State.Types (NPC(..))
import Game.Logic.Chest (Chest(..), ChestState(..))
import Game.Logic.Lookup

spec :: Spec
spec = do
  describe "monsterAt" $ do
    it "finds a monster at the given position" $ do
      let rat = mkMonster Rat (V2 3 3)
      monsterAt (V2 3 3) [rat] `shouldBe` Just (0, rat)

    it "returns Nothing for empty list" $
      monsterAt (V2 1 1) [] `shouldBe` Nothing

    it "returns Nothing when no monster at position" $ do
      let rat = mkMonster Rat (V2 3 3)
      monsterAt (V2 1 1) [rat] `shouldBe` Nothing

    it "returns the correct index for second monster" $ do
      let r1 = mkMonster Rat (V2 1 1)
          r2 = mkMonster Goblin (V2 3 3)
      case monsterAt (V2 3 3) [r1, r2] of
        Just (i, _) -> i `shouldBe` 1
        Nothing     -> expectationFailure "should have found goblin"

  describe "npcAt" $ do
    let npc = NPC "Test" (V2 2 2) "Hi" Nothing []

    it "finds an NPC at the given position" $
      npcAt (V2 2 2) [npc] `shouldBe` Just (0, npc)

    it "returns Nothing for wrong position" $
      npcAt (V2 1 1) [npc] `shouldBe` Nothing

  describe "chestAt" $ do
    let chest = Chest (V2 2 2) (ChestFull (IPotion HealingMinor))

    it "finds a chest at the given position" $
      case chestAt (V2 2 2) [chest] of
        Just (0, _) -> pure ()
        _           -> expectationFailure "should have found chest"

    it "returns Nothing for wrong position" $
      chestAt (V2 1 1) [chest] `shouldBe` Nothing

  describe "replaceChestAt" $ do
    let c1 = Chest (V2 1 1) (ChestFull (IPotion HealingMinor))
        c2 = Chest (V2 2 2) (ChestFull (IPotion HealingMajor))
        replacement = Chest (V2 1 1) (ChestEmpty 10)

    it "replaces the chest at the given index" $ do
      let result = replaceChestAt 0 replacement [c1, c2]
      head result `shouldBe` replacement
      result !! 1 `shouldBe` c2

    it "leaves list unchanged for out-of-range index" $ do
      let result = replaceChestAt 5 replacement [c1, c2]
      result `shouldBe` [c1, c2]

  describe "findKeyIndex" $ do
    it "finds a matching key" $ do
      let inv = emptyInventory { invItems = [IPotion HealingMinor, IKey (KeyId 1), IPotion HealingMajor] }
      findKeyIndex (KeyId 1) inv `shouldBe` Just 1

    it "returns Nothing when no matching key" $ do
      let inv = emptyInventory { invItems = [IPotion HealingMinor] }
      findKeyIndex (KeyId 1) inv `shouldBe` Nothing

    it "returns Nothing for empty inventory" $
      findKeyIndex (KeyId 1) emptyInventory `shouldBe` Nothing

  describe "takeFirstItemAt" $ do
    let items = [(V2 1 1, IPotion HealingMinor), (V2 2 2, IPotion HealingMajor), (V2 1 1, IPotion HealingMinor)]

    it "takes the first item at the position" $
      case takeFirstItemAt (V2 1 1) items of
        Just (item, rest) -> do
          item `shouldBe` IPotion HealingMinor
          length rest `shouldBe` 2
        Nothing -> expectationFailure "should have found item"

    it "returns Nothing when no item at position" $
      takeFirstItemAt (V2 3 3) items `shouldBe` Nothing

    it "returns Nothing for empty list" $
      takeFirstItemAt (V2 1 1) [] `shouldBe` Nothing
