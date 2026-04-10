module Game.Logic.Movement
  ( tryMove
  , DashContext(..)
  , dashContext
  , dashSteps
  ) where

import Game.Types
import Game.State.Types (GameState(..), NPC(..))
import Game.Logic.Lookup (monsterAt, npcAt)

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

-- | The subset of game state needed by 'dashSteps'.
data DashContext = DashContext
  { dcLevel     :: !DungeonLevel
  , dcMonsters  :: ![Monster]
  , dcNPCs      :: ![NPC]
  , dcItems     :: ![(Pos, Item)]
  , dcPlayerPos :: !Pos
  }

-- | Build a 'DashContext' from the live game state.
dashContext :: GameState -> DashContext
dashContext gs = DashContext
  { dcLevel     = gsLevel gs
  , dcMonsters  = gsMonsters gs
  , dcNPCs      = gsNPCs gs
  , dcItems     = gsItemsOnFloor gs
  , dcPlayerPos = gsPlayerPos gs
  }

-- | Accumulate up to @n@ successive positions in direction @dir@
--   starting from the given position. Stops as soon as the next step
--   would land on anything other than plain floor or an open door,
--   anything occupied by a monster / NPC, or anything with a floor
--   item on it. Returns the list of /positions stepped onto/ (empty
--   list = blocked at step 1).
dashSteps :: DashContext -> Dir -> Int -> [Pos]
dashSteps dc dir n =
  let dl       = dcLevel dc
      monsters = dcMonsters dc
      npcs     = dcNPCs dc
      items    = dcItems dc
      offset   = dirToOffset dir

      go 0 _ = []
      go k p =
        let next = p + offset
        in if not (dashPassable next)
             then []
             else next : go (k - 1) next

      dashPassable p =
        case tileAt dl p of
          Just Floor       -> clearOfActors p
          Just (Door Open) -> clearOfActors p
          _                -> False

      clearOfActors p =
           case monsterAt p monsters of { Just _ -> False; Nothing -> True }
        && case npcAt     p npcs     of { Just _ -> False; Nothing -> True }
        && not (any ((== p) . fst) items)

  in go n (dcPlayerPos dc)
