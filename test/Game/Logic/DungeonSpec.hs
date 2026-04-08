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
  let gen                    = mkStdGen seed
      (dl, start, rooms, _)  = generateLevel gen defaultLevelConfig
  in (dl, start, rooms)

-- | 4-connected flood fill over walkable tiles starting from a point.
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
                Just t | isWalkable t -> True
                _                     -> False
            ]
      in go (foldr Set.insert visited fresh) (qs ++ fresh)

-- | All walkable tile positions in a level.
walkableTiles :: DungeonLevel -> Set.Set Pos
walkableTiles dl = Set.fromList
  [ V2 x y
  | y <- [0 .. dlHeight dl - 1]
  , x <- [0 .. dlWidth  dl - 1]
  , let p = V2 x y
  , Just t <- [tileAt dl p]
  , isWalkable t
  ]

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
