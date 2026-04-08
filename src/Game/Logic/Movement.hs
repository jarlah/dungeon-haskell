module Game.Logic.Movement (tryMove) where

import Game.Types

-- | Attempt to move from a position one step in a direction.
--   Returns the new position if the destination tile exists and is
--   walkable; 'Nothing' otherwise (wall, closed door, out of bounds).
--
--   This is pure. All movement rules live here so they can be tested
--   with QuickCheck.
tryMove :: DungeonLevel -> Pos -> Dir -> Maybe Pos
tryMove dl pos dir =
  let newPos = pos + dirToOffset dir
  in case tileAt dl newPos of
       Just t | isWalkable t -> Just newPos
       _                     -> Nothing
