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

-- | Generate a random dungeon level.
--
--   Takes the run's current "next key id" counter and returns the
--   level, the player start position (on StairsUp), the list of
--   rooms placed, an optional @(KeyId, Pos)@ for the lock that got
--   minted on this floor (paired with the door's tile position so
--   callers can compute reachability around it), the updated
--   next-key counter, and the advanced 'StdGen'. The room list is
--   returned so callers can use it for monster / loot placement
--   without re-running 'placeRooms'.
--
--   The lock — if one was rolled — always lives on a door in a
--   non-spawn-room wall, so the player can never be sealed into
--   their starting room by a lock they can't open.
generateLevel
  :: StdGen
  -> LevelConfig
  -> Int
  -> (DungeonLevel, Pos, [Room], Maybe (KeyId, Pos), Int, StdGen)
generateLevel gen0 cfg nextKey0 =
  let (rooms,     gen1) = placeRooms     cfg gen0
      (corridors, gen2) = carveCorridors gen1 rooms
      w = lcWidth  cfg
      h = lcHeight cfg

      corridorSet :: Set.Set Pos
      corridorSet = Set.fromList corridors

      floorSet :: Set.Set Pos
      floorSet = Set.fromList (concatMap roomTiles rooms) `Set.union` corridorSet

      baseTiles = V.generate (w * h) $ \i ->
        let (y, x) = i `divMod` w
            p      = V2 x y
        in if Set.member p floorSet then Floor else Wall

      (stairsUpPos, stairsDownPos) = case rooms of
        []       -> (V2 1 1, V2 1 1)
        [r]      -> (roomCenter r, roomCenter r)
        (r : rs) -> (roomCenter r, roomCenter (last (r : rs)))

      -- Door placement: pick the tiles where a corridor has punched
      -- through a room's *outer wall* (the ring of tiles one step
      -- outside the room's floor area), keep only actual pinch
      -- points (floor on one pair of opposite neighbors, wall on
      -- the other), roll each survivor Open / Closed, then stamp
      -- them into the tile vector before stairs overwrite anything.
      -- Spawn room outer walls are forced Open so the player can
      -- never be softlocked into their starting area.
      spawnWalls :: Set.Set Pos
      spawnWalls = case rooms of
        (r : _) -> Set.fromList (roomOuterWallTiles r)
        []      -> Set.empty
      rawCandidates      =
        dedup (concatMap (roomDoorSites corridorSet) rooms)
      pinchPoints        =
        filter (isPinchPoint floorSet) rawCandidates
      candidates         =
        collapseAdjacentCandidates pinchPoints
      (doorStates0, gen3)                         = rollDoors gen2 spawnWalls candidates
      (doorStates, mLockedInfo, nextKey1, gen4)   =
        tryLockOneDoor gen3 nextKey0 spawnWalls doorStates0

      dedup :: [Pos] -> [Pos]
      dedup = Set.toAscList . Set.fromList

      tiles = baseTiles V.//
        ( [ (posToIdx w p, Door ds) | (p, ds) <- doorStates ]
          ++ [ (posToIdx w stairsUpPos,   StairsUp)
             , (posToIdx w stairsDownPos, StairsDown)
             ]
        )

      dl = DungeonLevel
        { dlWidth  = w
        , dlHeight = h
        , dlDepth  = lcDepth cfg
        , dlTiles  = tiles
        , dlRooms  = rooms
        }
  in (dl, stairsUpPos, rooms, mLockedInfo, nextKey1, gen4)

posToIdx :: Int -> Pos -> Int
posToIdx w (V2 x y) = y * w + x

-- | Walk a list of door candidates in order and drop any that are
--   cardinally adjacent to a candidate already kept. This collapses
--   the "doubled doors" that appear when a corridor punches through
--   a 2-tile-thick wall between rooms: both rooms contribute a
--   candidate at the punched-through tile, and both tiles survive
--   the pinch-point filter, so without this step the player sees
--   @+'@ or @''@ side by side.
collapseAdjacentCandidates :: [Pos] -> [Pos]
collapseAdjacentCandidates = go Set.empty
  where
    go _        []       = []
    go accepted (p : ps)
      | any (`Set.member` accepted) (neighbors4 p) = go accepted ps
      | otherwise =
          p : go (Set.insert p accepted) ps

    neighbors4 p =
      [ p + V2 0 (-1)
      , p + V2 0   1
      , p + V2 1   0
      , p + V2 (-1) 0
      ]

-- | Is the tile at 'p' a pinch point against a set of floor tiles
--   — floor on exactly one pair of opposite cardinal neighbors and
--   wall on the other pair? This is the geometric definition of
--   "a gap in a wall," which is what every door must be. Without
--   this check a corridor that passes through the thin strip of
--   wall between two adjacent rooms produces "doors" that are
--   actually surrounded by floor on all four sides.
isPinchPoint :: Set.Set Pos -> Pos -> Bool
isPinchPoint floorSet p =
  let fN = Set.member (p + V2 0 (-1)) floorSet
      fS = Set.member (p + V2 0   1 ) floorSet
      fE = Set.member (p + V2 1   0 ) floorSet
      fW = Set.member (p + V2 (-1) 0) floorSet
  in (fN && fS && not fE && not fW)
  || (fE && fW && not fN && not fS)

-- | The ring of tiles one step *outside* a room's floor area — the
--   tiles where the BSP generator ordinarily stamps wall. A door
--   placed here sits inside an intact wall segment and reads
--   visually as "a gap in the wall." Corner tiles appear in two of
--   the four segments returned but downstream users dedup.
roomOuterWallTiles :: Room -> [Pos]
roomOuterWallTiles (Room x y w h) =
  let xs = [x .. x + w - 1]
      ys = [y .. y + h - 1]
  in    [V2 xx (y - 1)       | xx <- xs]  -- top wall
     ++ [V2 xx (y + h)       | xx <- xs]  -- bottom wall
     ++ [V2 (x - 1)       yy | yy <- ys]  -- left wall
     ++ [V2 (x + w)       yy | yy <- ys]  -- right wall

-- | Door sites for a single room: scan each of the four *outer*
--   walls independently for corridor tiles and collapse runs of
--   orthogonally-adjacent hits to a single position. A "hit" is a
--   wall-ring tile the corridor carver turned into floor — i.e. an
--   actual gap in the wall — so the resulting doors always sit
--   between an interior floor tile and an exterior corridor tile,
--   never in the middle of a room. Scanning per wall guarantees a
--   corridor that runs parallel to a wall produces one door, not
--   a line of them.
roomDoorSites :: Set.Set Pos -> Room -> [Pos]
roomDoorSites corridorSet (Room x y w h) =
  let xs = [x .. x + w - 1]
      ys = [y .. y + h - 1]
      hits ps = collapseRuns [p | p <- ps, Set.member p corridorSet]
      top    = hits [V2 xx (y - 1)       | xx <- xs]
      bottom = hits [V2 xx (y + h)       | xx <- xs]
      left   = hits [V2 (x - 1)       yy | yy <- ys]
      right  = hits [V2 (x + w)       yy | yy <- ys]
  in top ++ bottom ++ left ++ right

-- | Walk a list of positions in order and collapse runs of
--   orthogonally-adjacent positions to the run's midpoint. Callers
--   pass positions that are already sorted along one axis (a row or
--   a column), so adjacency reduces to "previous position is one
--   step away along that axis."
collapseRuns :: [Pos] -> [Pos]
collapseRuns = map runMid . groupRuns
  where
    groupRuns []       = []
    groupRuns (p : ps) =
      let (run, rest) = spanAdj p ps
      in (p : run) : groupRuns rest

    spanAdj _ [] = ([], [])
    spanAdj prev (q : qs)
      | adjacent prev q =
          let (run, rest) = spanAdj q qs
          in (q : run, rest)
      | otherwise       = ([], q : qs)

    adjacent (V2 a b) (V2 c d) =
      (a == c && abs (b - d) == 1) || (b == d && abs (a - c) == 1)

    runMid run = run !! (length run `div` 2)

-- | Roll the state of every candidate door site from the level
--   'StdGen'. Sites on the spawn room's perimeter are always 'Open';
--   every other site is 'Open' with probability ~0.7 and 'Closed'
--   with probability ~0.3. Locking is a separate post-pass (see
--   'tryLockOneDoor') because it has to pick at most one door on
--   the whole level and the ordinary open/closed roll is per-site.
rollDoors :: StdGen -> Set.Set Pos -> [Pos] -> ([(Pos, DoorState)], StdGen)
rollDoors gen0 spawnPerim = go gen0
  where
    go g [] = ([], g)
    go g (p : ps) =
      let (ds, g') =
            if Set.member p spawnPerim
              then (Open, g)
              else let (r, g1) = randomR (1 :: Int, 10) g
                   in (if r <= 7 then Open else Closed, g1)
          (rest, g'') = go g' ps
      in ((p, ds) : rest, g'')

-- | With 50% probability, upgrade one of the rolled doors to
--   'Locked' and mint a fresh 'KeyId' for it. Only non-spawn-room
--   doors are eligible so the player can never be sealed into
--   their starting area. If the roll fails, or there are no
--   eligible doors, the door list is returned unchanged and the
--   key counter is untouched.
--
--   Returns the (possibly updated) door list, the 'KeyId' that
--   was minted (if any), the advanced next-key counter, and the
--   advanced 'StdGen'.
tryLockOneDoor
  :: StdGen
  -> Int
  -> Set.Set Pos
  -> [(Pos, DoorState)]
  -> ([(Pos, DoorState)], Maybe (KeyId, Pos), Int, StdGen)
tryLockOneDoor gen0 nextKey spawnPerim doors =
  let (roll, gen1) = randomR (1 :: Int, 100) gen0
      eligible     =
        [ i
        | (i, (p, _)) <- zip [0 :: Int ..] doors
        , not (Set.member p spawnPerim)
        ]
  in if roll > 50 || null eligible
       then (doors, Nothing, nextKey, gen1)
       else
         let (pickIdx, gen2) = randomR (0, length eligible - 1) gen1
             targetIdx       = eligible !! pickIdx
             kid             = KeyId nextKey
             targetPos       = fst (doors !! targetIdx)
             doors'          =
               [ if i == targetIdx then (p, Locked kid) else (p, ds)
               | (i, (p, ds)) <- zip [0 :: Int ..] doors
               ]
         in (doors', Just (kid, targetPos), nextKey + 1, gen2)


-- | Turn every 'StairsDown' tile on a level into plain 'Floor'.
--   Used by the boss floor: the run can't progress deeper than the
--   dragon, so we remove the downward stair before handing the
--   level to the player. Any other tile is left untouched.
stripStairsDown :: DungeonLevel -> DungeonLevel
stripStairsDown dl = dl { dlTiles = V.map swap (dlTiles dl) }
  where
    swap StairsDown = Floor
    swap t          = t
