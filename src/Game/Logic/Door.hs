-- | Door open/close operations.
module Game.Logic.Door
  ( openDoorAt
  , closeDoorAt
  ) where

import qualified Data.Vector as V
import Linear (V2(..))

import Game.Types (DungeonLevel(..), Pos, Tile(..), DoorState(..))

-- | Rewrite the tile at 'p' to @Door Open@. Used by the bump-to-open
--   path in 'applyAction' so a closed door becomes walkable on the
--   same turn the player tried to step onto it. Out-of-bounds
--   positions are a no-op (the caller only invokes this after
--   'tileAt' has already confirmed the tile is @Door Closed@).
openDoorAt :: Pos -> DungeonLevel -> DungeonLevel
openDoorAt (V2 x y) lvl =
  let w = dlWidth  lvl
      h = dlHeight lvl
  in if x < 0 || y < 0 || x >= w || y >= h
       then lvl
       else let idx      = y * w + x
                newTiles = dlTiles lvl V.// [(idx, Door Open)]
            in lvl { dlTiles = newTiles }

-- | Rewrite the tile at 'p' to @Door Closed@. Mirrors 'openDoorAt'
--   and is used by 'playerCloseDoor'. The caller is responsible
--   for confirming the tile is currently @Door Open@ and that
--   nothing stands on it.
closeDoorAt :: Pos -> DungeonLevel -> DungeonLevel
closeDoorAt (V2 x y) lvl =
  let w = dlWidth  lvl
      h = dlHeight lvl
  in if x < 0 || y < 0 || x >= w || y >= h
       then lvl
       else let idx      = y * w + x
                newTiles = dlTiles lvl V.// [(idx, Door Closed)]
            in lvl { dlTiles = newTiles }
