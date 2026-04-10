module Game.Logic.PlacementSpec (spec) where

import Test.Hspec
import qualified Data.Set as Set
import qualified Data.Vector as V
import Linear (V2(..))
import System.Random (mkStdGen)

import Game.Types (Pos, DungeonLevel(..), Tile(..), DoorState(..), KeyId(..), Item(..), tileAt)
import Game.Logic.Dungeon (Room(..))
import Game.Logic.Placement

spec :: Spec
spec = do
  describe "roomPositions" $ do
    it "returns all positions in a room" $ do
      let ps = roomPositions (Room 1 1 3 3)
      length ps `shouldBe` 9
      V2 1 1 `elem` ps `shouldBe` True
      V2 3 3 `elem` ps `shouldBe` True

    it "returns a single position for a 1x1 room" $ do
      let ps = roomPositions (Room 5 5 1 1)
      ps `shouldBe` [V2 5 5]

  describe "spawnSideReachable" $ do
    it "flood-fills all walkable tiles from spawn" $ do
      -- 5x5 all floor except walls
      let lvl = DungeonLevel
            { dlWidth = 5, dlHeight = 5, dlDepth = 1
            , dlTiles = V.generate 25 $ \i ->
                let (y, x) = i `divMod` 5
                in if x == 0 || y == 0 || x == 4 || y == 4 then Wall else Floor
            , dlRooms = [Room 1 1 3 3]
            }
          reachable = spawnSideReachable lvl (V2 2 2)
      -- 3x3 interior = 9 tiles
      Set.size reachable `shouldBe` 9

    it "treats locked doors as impassable" $ do
      -- 7x3 corridor with a locked door in the middle
      let kid = KeyId 1
          lvl = DungeonLevel
            { dlWidth = 7, dlHeight = 3, dlDepth = 1
            , dlTiles = V.generate (7 * 3) $ \i ->
                let (y, x) = i `divMod` 7
                in if y == 0 || y == 2 || x == 0 || x == 6 then Wall
                   else if x == 3 then Door (Locked kid)
                   else Floor
            , dlRooms = [Room 1 1 5 1]
            }
          reachable = spawnSideReachable lvl (V2 1 1)
      -- Left side: (1,1) and (2,1) are reachable; locked door at (3,1) blocks
      Set.member (V2 1 1) reachable `shouldBe` True
      Set.member (V2 2 1) reachable `shouldBe` True
      Set.member (V2 3 1) reachable `shouldBe` False
      Set.member (V2 4 1) reachable `shouldBe` False

  describe "placeKeyLoot" $ do
    it "places one key per KeyId" $ do
      let rooms = [Room 1 1 3 3, Room 5 1 3 3]
          reachable = Set.fromList [V2 x y | x <- [1..3], y <- [1..3]]
          keys = [KeyId 1, KeyId 2]
          (loot, _) = placeKeyLoot (mkStdGen 42) reachable rooms keys
      length loot `shouldBe` 2

    it "only places keys on reachable tiles" $ do
      let rooms = [Room 1 1 3 3]
          reachable = Set.fromList [V2 1 1, V2 2 1]
          keys = [KeyId 1]
          (loot, _) = placeKeyLoot (mkStdGen 42) reachable rooms keys
      case loot of
        [(pos, _)] -> Set.member pos reachable `shouldBe` True
        _          -> expectationFailure "expected exactly one key"

  describe "placeChests" $ do
    it "places up to n chests" $ do
      let lvl = DungeonLevel
            { dlWidth = 5, dlHeight = 5, dlDepth = 1
            , dlTiles = V.generate 25 $ \i ->
                let (y, x) = i `divMod` 5
                in if x == 0 || y == 0 || x == 4 || y == 4 then Wall else Floor
            , dlRooms = [Room 1 1 3 3]
            }
          rooms = [Room 1 1 3 3]
          (chests, _) = placeChests (mkStdGen 42) lvl rooms [] 3
      length chests `shouldSatisfy` (<= 3)
      length chests `shouldSatisfy` (> 0)

    it "returns empty for zero requested" $ do
      let lvl = DungeonLevel
            { dlWidth = 5, dlHeight = 5, dlDepth = 1
            , dlTiles = V.replicate 25 Floor
            , dlRooms = [Room 1 1 3 3]
            }
          (chests, _) = placeChests (mkStdGen 42) lvl [Room 1 1 3 3] [] 0
      chests `shouldBe` []

  describe "pickBossTopLeft" $ do
    it "returns a position inside the room" $ do
      let room = Room 5 5 4 4
          (V2 x y, _) = pickBossTopLeft room (mkStdGen 42)
      x `shouldSatisfy` (>= 5)
      x `shouldSatisfy` (<= 7)  -- rX + rW - 2
      y `shouldSatisfy` (>= 5)
      y `shouldSatisfy` (<= 7)
