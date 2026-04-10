module Game.Logic.DoorSpec (spec) where

import Test.Hspec
import qualified Data.Vector as V
import Linear (V2(..))

import Game.Types
  ( Pos, Dir(..), DungeonLevel(..), Tile(..), DoorState(..), Item(..), Potion(..)
  , Monster, MonsterKind(..), mkMonster, tileAt
  )
import Game.State.Types (NPC(..))
import Game.Logic.Dungeon (Room(..))
import Game.Logic.Door (openDoorAt, closeDoorAt, dashSteps)

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

-- | Place a closed door at (2,1) in the tinyLevel.
withClosedDoor :: DungeonLevel
withClosedDoor =
  let idx = 1 * 5 + 2  -- y=1, x=2
  in tinyLevel { dlTiles = dlTiles tinyLevel V.// [(idx, Door Closed)] }

spec :: Spec
spec = do
  describe "openDoorAt" $ do
    it "changes a closed door to open" $ do
      let lvl' = openDoorAt (V2 2 1) withClosedDoor
      tileAt lvl' (V2 2 1) `shouldBe` Just (Door Open)

    it "is a no-op for out-of-bounds" $ do
      let lvl' = openDoorAt (V2 99 99) withClosedDoor
      lvl' `shouldBe` withClosedDoor

    it "is a no-op for negative coords" $ do
      let lvl' = openDoorAt (V2 (-1) (-1)) withClosedDoor
      lvl' `shouldBe` withClosedDoor

  describe "closeDoorAt" $ do
    it "changes an open door to closed" $ do
      let withOpen = openDoorAt (V2 2 1) withClosedDoor
          lvl'    = closeDoorAt (V2 2 1) withOpen
      tileAt lvl' (V2 2 1) `shouldBe` Just (Door Closed)

  describe "dashSteps" $ do
    let noMonsters = [] :: [Monster]
        noNPCs     = [] :: [NPC]
        noItems    = [] :: [(Pos, Item)]

    it "returns positions up to the wall" $ do
      let steps = dashSteps tinyLevel noMonsters noNPCs noItems (V2 1 2) E 10
      -- from (1,2) going East: (2,2), (3,2) then wall at (4,2)
      steps `shouldBe` [V2 2 2, V2 3 2]

    it "returns empty when immediately blocked" $ do
      let steps = dashSteps tinyLevel noMonsters noNPCs noItems (V2 1 2) W 10
      -- from (1,2) going West: wall at (0,2)
      steps `shouldBe` []

    it "stops before a monster" $ do
      let rat   = mkMonster Rat (V2 3 2)
          steps = dashSteps tinyLevel [rat] noNPCs noItems (V2 1 2) E 10
      steps `shouldBe` [V2 2 2]

    it "stops before an item on the floor" $ do
      let items = [(V2 3 2, IPotion HealingMinor)]
          steps = dashSteps tinyLevel noMonsters noNPCs items (V2 1 2) E 10
      steps `shouldBe` [V2 2 2]

    it "respects the step limit" $ do
      -- 7x1 corridor (all floor)
      let wide = DungeonLevel
            { dlWidth = 7, dlHeight = 3, dlDepth = 1
            , dlTiles = V.generate (7 * 3) $ \i ->
                let (y, x) = i `divMod` 7
                in if y == 0 || y == 2 || x == 0 || x == 6 then Wall else Floor
            , dlRooms = [Room 1 1 5 1]
            }
          steps = dashSteps wide noMonsters noNPCs noItems (V2 1 1) E 2
      length steps `shouldBe` 2

    it "passes through an open door" $ do
      let withOpen = tinyLevel
            { dlTiles = dlTiles tinyLevel V.// [(2 * 5 + 2, Door Open)] }
          steps = dashSteps withOpen noMonsters noNPCs noItems (V2 1 2) E 10
      -- (2,2) is open door, (3,2) is floor, (4,2) is wall
      steps `shouldBe` [V2 2 2, V2 3 2]

    it "stops at a closed door" $ do
      let withDoor = tinyLevel
            { dlTiles = dlTiles tinyLevel V.// [(2 * 5 + 2, Door Closed)] }
          steps = dashSteps withDoor noMonsters noNPCs noItems (V2 1 2) E 10
      steps `shouldBe` []
