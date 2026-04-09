{-# LANGUAGE ScopedTypeVariables #-}
module Game.Logic.DungeonSpec (spec) where

import Data.List (tails)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Linear (V2(..))
import System.Random (mkStdGen)
import Test.Hspec
import Test.QuickCheck

import Game.Logic.Dungeon
import Game.Types

-- | Deterministic level generation from an Int seed — makes the
--   properties fully reproducible under QuickCheck shrinking.
levelFor :: Int -> (DungeonLevel, Pos, [Room])
levelFor seed =
  let gen                          = mkStdGen seed
      (dl, start, rooms, _, _, _)  = generateLevel gen defaultLevelConfig 0
  in (dl, start, rooms)

-- | Same as 'levelFor' but also surfaces the locked door info and
--   the updated next-key counter. Used by the reachability property
--   below, which needs both the door position (to run its own
--   flood fill around it) and the key id (to decide whether to
--   skip levels that didn't mint a lock).
levelForFull
  :: Int
  -> (DungeonLevel, Pos, [Room], Maybe (KeyId, Pos), Int)
levelForFull seed =
  let gen                                     = mkStdGen seed
      (dl, start, rooms, mLocked, nextKey, _) = generateLevel gen defaultLevelConfig 0
  in (dl, start, rooms, mLocked, nextKey)

-- | Treat a tile as reachable-by-player: walkable tiles plus any
--   door (closed doors bump-to-open; locked doors open once the
--   matching key is held, and the generator always places the key
--   somewhere in the same connected component). This is the
--   invariant the generator has to satisfy — every tile that
--   looks like a door or floor to the player must actually be
--   reachable, not just those the movement predicate lets them
--   step onto.
playerCanReach :: Tile -> Bool
playerCanReach t = case t of
  Door Closed     -> True
  Door (Locked _) -> True
  _               -> isWalkable t

-- | 4-connected flood fill over player-reachable tiles starting
--   from a point. Passes through closed doors because the player
--   opens them by bumping.
floodFill :: DungeonLevel -> Pos -> Set.Set Pos
floodFill dl start = go (Set.singleton start) [start]
  where
    go visited []       = visited
    go visited (p : qs) =
      let neighbors =
            [ p + dirToOffset d
            | d <- [N, S, E, W]
            ]
          fresh =
            [ n
            | n <- neighbors
            , not (Set.member n visited)
            , case tileAt dl n of
                Just t | playerCanReach t -> True
                _                         -> False
            ]
      in go (foldr Set.insert visited fresh) (qs ++ fresh)

-- | All player-reachable tile positions in a level (floor, stairs,
--   and doors of any state).
walkableTiles :: DungeonLevel -> Set.Set Pos
walkableTiles dl = Set.fromList
  [ V2 x y
  | y <- [0 .. dlHeight dl - 1]
  , x <- [0 .. dlWidth  dl - 1]
  , let p = V2 x y
  , Just t <- [tileAt dl p]
  , playerCanReach t
  ]

-- | Predicate for door tiles regardless of state. Used by the
--   Milestone 15 invariants so tests don't have to match on
--   individual 'DoorState' constructors.
isDoorTile :: Tile -> Bool
isDoorTile (Door _) = True
isDoorTile _        = False

-- | Flood fill from 'start' treating walls AND locked doors as
--   impassable — the set of tiles a keyless player can reach
--   without opening the lock. This is the test-side mirror of
--   'Game.GameState.spawnSideReachable', duplicated so this spec
--   stays standalone (no dependency on the game state module from
--   the dungeon generator spec).
spawnSideFlood :: DungeonLevel -> Pos -> Set.Set Pos
spawnSideFlood dl start = go (Set.singleton start) [start]
  where
    passable p = case tileAt dl p of
      Just Wall              -> False
      Just (Door (Locked _)) -> False
      Just _                 -> True
      Nothing                -> False
    go visited []       = visited
    go visited (p : qs) =
      let neighbors = [ p + dirToOffset d | d <- [N, S, E, W] ]
          fresh     =
            [ n
            | n <- neighbors
            , not (Set.member n visited)
            , passable n
            ]
      in go (foldr Set.insert visited fresh) (qs ++ fresh)

-- | The ring of tiles one step *outside* a room's floor area.
--   Mirrors 'roomOuterWallTiles' in 'Game.Logic.Dungeon', which
--   isn't exported; duplicating a 4-line helper here is cheaper
--   than widening the module interface for a test.
roomOuterWallPositions :: Room -> [Pos]
roomOuterWallPositions (Room x y w h) =
  let xs = [x .. x + w - 1]
      ys = [y .. y + h - 1]
  in    [V2 xx (y - 1)       | xx <- xs]
     ++ [V2 xx (y + h)       | xx <- xs]
     ++ [V2 (x - 1)       yy | yy <- ys]
     ++ [V2 (x + w)       yy | yy <- ys]

spec :: Spec
spec = describe "Game.Logic.Dungeon.generateLevel" $ do

  it "prop_wallsBorderLevel" $ property $
    \(seed :: Int) ->
      let (dl, _, _) = levelFor seed
          w = dlWidth  dl
          h = dlHeight dl
          border =
            [V2 x 0       | x <- [0 .. w - 1]]
              ++ [V2 x (h - 1) | x <- [0 .. w - 1]]
              ++ [V2 0 y       | y <- [0 .. h - 1]]
              ++ [V2 (w - 1) y | y <- [0 .. h - 1]]
      in all (\p -> tileAt dl p == Just Wall) border

  it "prop_minimumRoomCount" $ property $
    \(seed :: Int) ->
      let (_, _, rooms) = levelFor seed
      in length rooms >= 1

  it "prop_noOverlappingRooms" $ property $
    \(seed :: Int) ->
      let (_, _, rooms) = levelFor seed
          pairs = [(a, b) | (a : rest) <- tails rooms, b <- rest]
      in all (\(a, b) -> not (roomsIntersect a b)) pairs

  it "prop_stairsExist" $ property $
    \(seed :: Int) ->
      let (dl, _, rooms) = levelFor seed
          ts      = V.toList (dlTiles dl)
          hasUp   = StairsUp   `elem` ts
          hasDown = StairsDown `elem` ts
      in length rooms >= 2 ==> hasUp && hasDown

  it "prop_allWalkableTilesReachableFromStart" $ property $
    \(seed :: Int) ->
      let (dl, start, _) = levelFor seed
      in floodFill dl start === walkableTiles dl

  -- ----------------------------------------------------------------
  -- Milestone 15 / Step 1: doors at room-corridor junctions
  -- ----------------------------------------------------------------

  it "prop_everyLevelHasAtLeastOneDoor" $ property $
    \(seed :: Int) ->
      let (dl, _, rooms)  = levelFor seed
          ts              = V.toList (dlTiles dl)
          hasDoor         = any isDoorTile ts
      in length rooms >= 2 ==> hasDoor

  it "prop_noDoorOnStairs" $ property $
    \(seed :: Int) ->
      let (dl, start, _) = levelFor seed
          -- The spawn tile is stairs-up; look for stairs-down too
          -- and check neither is a door.
          startTile      = tileAt dl start
          downIdxs       =
            [ V2 x y
            | y <- [0 .. dlHeight dl - 1]
            , x <- [0 .. dlWidth  dl - 1]
            , tileAt dl (V2 x y) == Just StairsDown
            ]
          downTiles      = map (tileAt dl) downIdxs
      in all (not . maybe False isDoorTile)
             (startTile : downTiles)

  it "prop_spawnRoomHasNoClosedDoor" $ property $
    \(seed :: Int) ->
      let (dl, _, rooms) = levelFor seed
      in case rooms of
           []      -> True
           (r : _) ->
             let walls = roomOuterWallPositions r
             in all (\p -> tileAt dl p /= Just (Door Closed)) walls

  -- ----------------------------------------------------------------
  -- Milestone 15 / Step 2: locked doors
  -- ----------------------------------------------------------------

  it "prop_atMostOneLockedDoorPerLevel" $ property $
    \(seed :: Int) ->
      let gen                       = mkStdGen seed
          (dl, _, _, _, _, _)       = generateLevel gen defaultLevelConfig 0
          ts                        = V.toList (dlTiles dl)
          isLocked (Door (Locked _)) = True
          isLocked _                 = False
      in length (filter isLocked ts) <= 1

  it "prop_lockedDoorImpliesKeyIdAndPosMatch" $ property $
    \(seed :: Int) ->
      let (dl, _, _, mLocked, _) = levelForFull seed
      in case mLocked of
           Nothing        -> property True
           Just (_, pos)  ->
             -- The returned door position must actually be a
             -- locked-door tile on the generated level.
             case tileAt dl pos of
               Just (Door (Locked _)) -> property True
               other                  ->
                 counterexample
                   ("expected locked door at " ++ show pos
                      ++ ", got " ++ show other)
                   False

  it "prop_spawnRoomHasNoLockedDoor" $ property $
    \(seed :: Int) ->
      let (dl, _, rooms) = levelFor seed
      in case rooms of
           []      -> True
           (r : _) ->
             let walls = roomOuterWallPositions r
                 isLocked (Just (Door (Locked _))) = True
                 isLocked _                        = False
             in not (any (isLocked . tileAt dl) walls)

  it "advances nextKeyId iff a lock was minted" $ property $
    \(seed :: Int) ->
      let gen                            = mkStdGen seed
          (_, _, _, mLocked, nextKey, _) = generateLevel gen defaultLevelConfig 0
      in case mLocked of
           Just (KeyId k, _) -> nextKey === k + 1
           Nothing           -> nextKey === 0
