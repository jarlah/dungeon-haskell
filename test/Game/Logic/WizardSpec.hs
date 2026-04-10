module Game.Logic.WizardSpec (spec) where

import Test.Hspec
import qualified Data.Set as Set
import qualified Data.Vector as V
import Linear (V2(..))

import Game.Types
  ( DungeonLevel(..), Stats(..), MonsterKind(..)
  , Tile(..), mkMonster
  )
import Game.Logic.Dungeon (Room(..))
import Game.Logic.Wizard

-- | 5x5 room with walls around the border, floor inside.
tinyLevel :: DungeonLevel
tinyLevel = DungeonLevel
  { dlWidth  = 5
  , dlHeight = 5
  , dlDepth  = 1
  , dlTiles  = V.generate (5 * 5) $ \i ->
      let (y, x) = i `divMod` 5
      in if x == 0 || y == 0 || x == 4 || y == 4 then Wall else Floor
  , dlRooms  = [Room 1 1 3 3]
  }

baseStats :: Stats
baseStats = Stats
  { sHP = 10, sMaxHP = 25, sAttack = 6, sDefense = 2
  , sSpeed = 10, sLevel = 1, sXP = 0
  }

spec :: Spec
spec = do
  describe "wizCmdReveal" $ do
    it "adds all tiles to the explored set" $ do
      let explored = Set.empty
          result   = wizCmdReveal tinyLevel explored
      Set.size result `shouldBe` 25  -- 5x5

    it "is idempotent" $ do
      let full   = wizCmdReveal tinyLevel Set.empty
          again  = wizCmdReveal tinyLevel full
      again `shouldBe` full

  describe "wizCmdHeal" $ do
    it "sets HP to maxHP" $ do
      let healed = wizCmdHeal baseStats
      sHP healed `shouldBe` sMaxHP baseStats

    it "is a no-op at full HP" $ do
      let full   = baseStats { sHP = sMaxHP baseStats }
          healed = wizCmdHeal full
      healed `shouldBe` full

  describe "wizCmdKillAll" $ do
    it "returns count and empty list" $ do
      let monsters = [mkMonster Rat (V2 2 2), mkMonster Goblin (V2 3 3)]
          (n, ms)  = wizCmdKillAll monsters
      n `shouldBe` 2
      ms `shouldBe` []

    it "returns 0 for no monsters" $ do
      let (n, ms) = wizCmdKillAll []
      n `shouldBe` 0
      ms `shouldBe` []

  describe "wizCmdTeleport" $ do
    it "succeeds on a walkable tile" $
      wizCmdTeleport (V2 2 2) tinyLevel `shouldBe` Right (V2 2 2)

    it "fails on a wall" $
      case wizCmdTeleport (V2 0 0) tinyLevel of
        Left _  -> pure ()
        Right _ -> expectationFailure "should have failed on wall"

    it "fails on out-of-bounds" $
      case wizCmdTeleport (V2 10 10) tinyLevel of
        Left _  -> pure ()
        Right _ -> expectationFailure "should have failed OOB"

  describe "wizCmdSpawn" $ do
    it "spawns on an empty adjacent tile" $ do
      let playerPos = V2 2 2
      case wizCmdSpawn Rat playerPos tinyLevel [] of
        Right ms -> length ms `shouldBe` 1
        Left msg -> expectationFailure msg

    it "fails when all adjacent tiles are occupied" $ do
      let playerPos = V2 2 2
          -- Fill all 8 neighbors with monsters
          neighbors = [ V2 x y | x <- [1..3], y <- [1..3], V2 x y /= playerPos ]
          monsters  = map (mkMonster Rat) neighbors
      case wizCmdSpawn Rat playerPos tinyLevel monsters of
        Left _  -> pure ()
        Right _ -> expectationFailure "should have failed when boxed in"

  describe "wizCmdXP" $ do
    it "rejects negative XP" $
      case wizCmdXP (-1) baseStats of
        Left _  -> pure ()
        Right _ -> expectationFailure "should have rejected negative XP"

    it "grants XP and returns updated stats" $ do
      case wizCmdXP 10 baseStats of
        Right (s', _, _) -> sXP s' `shouldBe` sXP baseStats + 10
        Left msg         -> expectationFailure msg

    it "returns level-up messages when crossing a threshold" $ do
      let stats = baseStats { sXP = 99 }  -- close to level 2
      case wizCmdXP 100 stats of
        Right (s', msgs, evs) -> do
          sLevel s' `shouldSatisfy` (> sLevel stats)
          msgs `shouldSatisfy` (not . null)
          evs `shouldSatisfy` (not . null)
        Left msg -> expectationFailure msg
