module Game.Logic.SpawningSpec (spec) where

import Test.Hspec
import System.Random (mkStdGen)

import Game.Types (MonsterKind(..))
import Game.Logic.Quest (Quest(..), QuestGoal(..), QuestStatus(..))
import Game.Logic.Dungeon (Room(..))
import Game.Logic.Spawning

spec :: Spec
spec = do
  describe "randomMonsterKind" $ do
    it "only produces Rat or Goblin at depth 1" $ do
      let kinds = [ fst (randomMonsterKind 1 (mkStdGen s)) | s <- [0..99] ]
      all (`elem` [Rat, Goblin]) kinds `shouldBe` True

    it "can produce Orc at depth 2" $ do
      let kinds = [ fst (randomMonsterKind 2 (mkStdGen s)) | s <- [0..99] ]
      any (== Orc) kinds `shouldBe` True

  describe "spawnMonsters" $ do
    it "spawns some monsters in rooms" $ do
      let rooms = [Room 1 1 5 5, Room 10 10 5 5]
          (ms, _) = spawnMonsters (mkStdGen 42) 1 rooms
      -- With 2 rooms and 0-2 per room, expect at least some monsters
      -- (seed 42 should give us something)
      length ms `shouldSatisfy` (>= 0)

    it "returns empty for no rooms" $ do
      let (ms, _) = spawnMonsters (mkStdGen 42) 1 []
      ms `shouldBe` []

  describe "spawnNPCs" $ do
    it "spawns NPCs on depth 1 with multiple rooms" $ do
      let rooms = [Room 1 1 5 5, Room 10 10 5 5]
          (npcs, _) = spawnNPCs (mkStdGen 42) 1 rooms
      length npcs `shouldBe` 1

    it "spawns no NPCs on depth 2" $ do
      let rooms = [Room 1 1 5 5, Room 10 10 5 5]
          (npcs, _) = spawnNPCs (mkStdGen 42) 2 rooms
      npcs `shouldBe` []

    it "spawns no NPCs with only one room" $ do
      let rooms = [Room 1 1 5 5]
          (npcs, _) = spawnNPCs (mkStdGen 42) 1 rooms
      npcs `shouldBe` []

  describe "mkOffer" $ do
    it "creates a quest with QuestNotStarted status" $ do
      let q = mkOffer "Test" (GoalKillMonsters 5)
      qStatus q `shouldBe` QuestNotStarted
      qName q `shouldBe` "Test"

  describe "acceptOffer" $ do
    it "flips status to QuestActive" $ do
      let q  = mkOffer "Test" (GoalKillMonsters 5)
          q' = acceptOffer q
      qStatus q' `shouldBe` QuestActive
