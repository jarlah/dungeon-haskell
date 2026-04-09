module Game.Logic.Dungeon
  ( LevelConfig(..)
  , defaultLevelConfig
    -- Re-exported from "Game.Types" so callers can keep importing
    -- 'Room' from the dungeon module without a second import.
  , Room(..)
  , roomCenter
  , roomsIntersect
  , placeRooms
  , generateLevel
  , stripStairsDown
  ) where

import qualified Data.Set as Set
import qualified Data.Vector as V
import Linear (V2(..))
import System.Random (StdGen, random, randomR)

import Game.Types

-- | Knobs for a single-level generator run.
data LevelConfig = LevelConfig
  { lcWidth          :: !Int
  , lcHeight         :: !Int
  , lcMaxRooms       :: !Int   -- ^ max placement attempts / rooms
  , lcRoomMin        :: !Int   -- ^ min room edge length
  , lcRoomMax        :: !Int   -- ^ max room edge length
  , lcDepth          :: !Int
  , lcBossDepthRange :: !(Int, Int)
    -- ^ inclusive range of depths at which the boss encounter may
    --   appear. 'newGame' rolls a concrete 'gsBossDepth' once from
    --   this range so every run has a single, fixed boss floor.
  } deriving (Eq, Show)

defaultLevelConfig :: LevelConfig
defaultLevelConfig = LevelConfig
  { lcWidth          = 60
  , lcHeight         = 20
  , lcMaxRooms       = 14
  , lcRoomMin        = 4
  , lcRoomMax        = 9
  , lcDepth          = 1
  , lcBossDepthRange = (9, 11)
  }

-- 'Room' is defined in "Game.Types" so 'DungeonLevel' can carry it
-- without a module cycle. It's re-exported from here so existing
-- callers that @import Game.Logic.Dungeon (Room(..))@ keep working.

roomCenter :: Room -> Pos
roomCenter (Room x y w h) = V2 (x + w `div` 2) (y + h `div` 2)

-- | Do two rooms overlap or abut, including the 1-tile wall buffer?
roomsIntersect :: Room -> Room -> Bool
roomsIntersect a b =
  rX a     <= rX b + rW b &&
  rX a + rW a >= rX b     &&
  rY a     <= rY b + rH b &&
  rY a + rH a >= rY b

-- | Floor positions contained in a room.
roomTiles :: Room -> [Pos]
roomTiles (Room x y w h) =
  [V2 cx cy | cx <- [x .. x + w - 1], cy <- [y .. y + h - 1]]

-- | Pick a random room within the level bounds. May overlap existing rooms;
--   the caller is responsible for rejecting collisions.
randomRoom :: LevelConfig -> StdGen -> (Room, StdGen)
randomRoom cfg gen0 =
  let (w, gen1) = randomR (lcRoomMin cfg, lcRoomMax cfg) gen0
      (h, gen2) = randomR (lcRoomMin cfg, lcRoomMax cfg) gen1
      (x, gen3) = randomR (1, lcWidth  cfg - w - 2) gen2
      (y, gen4) = randomR (1, lcHeight cfg - h - 2) gen3
  in (Room x y w h, gen4)

-- | Place up to @lcMaxRooms@ non-overlapping rooms by random attempts.
placeRooms :: LevelConfig -> StdGen -> ([Room], StdGen)
placeRooms cfg = go (0 :: Int) []
  where
    go i acc gen
      | i >= lcMaxRooms cfg = (reverse acc, gen)
      | otherwise =
          let (room, gen') = randomRoom cfg gen
          in if any (roomsIntersect room) acc
               then go (i + 1) acc       gen'
               else go (i + 1) (room:acc) gen'

-- | Carve an L-shaped corridor between two points. Randomly picks whether
--   to go horizontal-first or vertical-first.
carveCorridor :: StdGen -> Pos -> Pos -> ([Pos], StdGen)
carveCorridor gen0 (V2 x1 y1) (V2 x2 y2) =
  let (horiFirst, gen1) = random gen0 :: (Bool, StdGen)
      hSeg yLine = [V2 x yLine | x <- rangeTo x1 x2]
      vSeg xLine = [V2 xLine y | y <- rangeTo y1 y2]
  in if horiFirst
       then (hSeg y1 ++ vSeg x2, gen1)
       else (vSeg x1 ++ hSeg y2, gen1)
  where
    rangeTo a b = [min a b .. max a b]

-- | Carve corridors connecting consecutive room centers.
carveCorridors :: StdGen -> [Room] -> ([Pos], StdGen)
carveCorridors gen []        = ([], gen)
carveCorridors gen [_]       = ([], gen)
carveCorridors gen0 (r1:r2:rs) =
  let (seg,  gen1) = carveCorridor gen0 (roomCenter r1) (roomCenter r2)
      (rest, gen2) = carveCorridors  gen1 (r2 : rs)
  in (seg ++ rest, gen2)

-- | Generate a random dungeon level. Returns the level, the player start
--   position (on StairsUp), the list of rooms placed, and the advanced
--   'StdGen'. The room list is returned so callers can use it for
--   monster / loot placement without re-running 'placeRooms'.
generateLevel :: StdGen -> LevelConfig -> (DungeonLevel, Pos, [Room], StdGen)
generateLevel gen0 cfg =
  let (rooms,     gen1) = placeRooms     cfg gen0
      (corridors, gen2) = carveCorridors gen1 rooms
      w = lcWidth  cfg
      h = lcHeight cfg

      floorSet :: Set.Set Pos
      floorSet = Set.fromList (concatMap roomTiles rooms ++ corridors)

      baseTiles = V.generate (w * h) $ \i ->
        let (y, x) = i `divMod` w
            p      = V2 x y
        in if Set.member p floorSet then Floor else Wall

      (stairsUpPos, stairsDownPos) = case rooms of
        []       -> (V2 1 1, V2 1 1)
        [r]      -> (roomCenter r, roomCenter r)
        (r : rs) -> (roomCenter r, roomCenter (last (r : rs)))

      tiles = baseTiles V.//
        [ (posToIdx w stairsUpPos,   StairsUp)
        , (posToIdx w stairsDownPos, StairsDown)
        ]

      dl = DungeonLevel
        { dlWidth  = w
        , dlHeight = h
        , dlDepth  = lcDepth cfg
        , dlTiles  = tiles
        , dlRooms  = rooms
        }
  in (dl, stairsUpPos, rooms, gen2)

posToIdx :: Int -> Pos -> Int
posToIdx w (V2 x y) = y * w + x

-- | Turn every 'StairsDown' tile on a level into plain 'Floor'.
--   Used by the boss floor: the run can't progress deeper than the
--   dragon, so we remove the downward stair before handing the
--   level to the player. Any other tile is left untouched.
stripStairsDown :: DungeonLevel -> DungeonLevel
stripStairsDown dl = dl { dlTiles = V.map swap (dlTiles dl) }
  where
    swap StairsDown = Floor
    swap t          = t
