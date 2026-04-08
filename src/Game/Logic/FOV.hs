-- | Field-of-view computation: what the player can see from a given
--   position, through a dungeon level, within a given radius.
--
--   The algorithm is Bresenham line-of-sight with a symmetric
--   fallback: @A@ sees @B@ if either the line from A→B OR the line
--   from B→A is unobstructed. Symmetrising the check makes
--   @prop_fovSymmetry@ trivially hold regardless of which endpoint
--   the line algorithm favors at diagonal ties.
--
--   This is pure and deterministic. It does not touch the game state
--   or any IO.
module Game.Logic.FOV
  ( computeFOV
  , transparent
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
import Linear (V2(..))

import Game.Types

-- | A tile is transparent iff light can pass through it. Closed
--   doors and walls block sight; open doors, floors, and stairs do
--   not.
transparent :: Tile -> Bool
transparent Floor         = True
transparent Wall          = False
transparent (Door Open)   = True
transparent (Door Closed) = False
transparent StairsDown    = True
transparent StairsUp      = True

-- | All tile positions visible from @origin@ within the given
--   Euclidean radius. The origin itself is always visible. A wall
--   at the far end of an otherwise-clear line of sight IS visible
--   (so the player can see the walls they're looking at).
computeFOV :: DungeonLevel -> Pos -> Int -> Set Pos
computeFOV dl origin@(V2 ox oy) radius =
  Set.fromList $ origin :
    [ p
    | dy <- [-radius .. radius]
    , dx <- [-radius .. radius]
    , dx * dx + dy * dy <= radius * radius
    , let p = V2 (ox + dx) (oy + dy)
    , p /= origin
    , inBounds p
    , canSee p
    ]
  where
    inBounds (V2 x y) =
      x >= 0 && y >= 0 && x < dlWidth dl && y < dlHeight dl

    -- Symmetric line-of-sight check.
    canSee target =
      losClear (bresenham origin target) ||
      losClear (bresenham target origin)

    -- A bresenham line is "clear" for FOV purposes if every tile
    -- STRICTLY between the start and the end is transparent. The
    -- start is the viewer (trivially fine) and the end is the
    -- target (may be opaque — we can see a wall we're looking at).
    losClear []        = False
    losClear [_]       = True
    losClear [_, _]    = True
    losClear (_ : mid) = checkMiddle mid

    checkMiddle [_]        = True               -- last element is the target
    checkMiddle (p : rest) = case tileAt dl p of
      Just t | transparent t -> checkMiddle rest
      _                      -> False
    checkMiddle []         = True

-- | Bresenham's line algorithm in 2D, integer grid, endpoints
--   inclusive. Returns the list of cells from the start to the end,
--   in order.
bresenham :: Pos -> Pos -> [Pos]
bresenham (V2 x0 y0) (V2 x1 y1) = go x0 y0 (dx + dy)
  where
    dx =  abs (x1 - x0)
    dy = -(abs (y1 - y0))
    sx = if x0 < x1 then 1 else -1
    sy = if y0 < y1 then 1 else -1
    go x y err
      | x == x1 && y == y1 = [V2 x y]
      | otherwise =
          let e2           = 2 * err
              (x', errX)   = if e2 >= dy then (x + sx, dy) else (x, 0)
              (y', errY)   = if e2 <= dx then (y + sy, dx) else (y, 0)
          in V2 x y : go x' y' (err + errX + errY)
