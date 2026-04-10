-- | Per-turn tick functions.
module Game.Logic.Tick
  ( tickDash
  , RegenContext(..)
  , regenContext
  , tickRegen
  , tickTurnCounter
  ) where

import qualified Data.Set as Set
import Data.Set (Set)

import Game.Types (Pos, Stats(..), Monster, monsterTiles)
import Game.State.Types (GameState(..))
import Game.Logic.Constants (regenInterval)

-- | Decrement dash cooldown by one if positive, otherwise no-op.
tickDash :: Int -> Int
tickDash cd
  | cd > 0    = cd - 1
  | otherwise = cd

-- | The subset of game state needed by 'tickRegen'.
data RegenContext = RegenContext
  { rcDead     :: !Bool
  , rcVictory  :: !Bool
  , rcStats    :: !Stats
  , rcCounter  :: !Int
  , rcVisible  :: !(Set Pos)
  , rcMonsters :: ![Monster]
  }

-- | Build a 'RegenContext' from the live game state.
regenContext :: GameState -> RegenContext
regenContext gs = RegenContext
  { rcDead     = gsDead gs
  , rcVictory  = gsVictory gs
  , rcStats    = gsPlayerStats gs
  , rcCounter  = gsRegenCounter gs
  , rcVisible  = gsVisible gs
  , rcMonsters = gsMonsters gs
  }

-- | Passive HP regen tick. Returns @(newStats, newRegenCounter)@.
--
--   While no hostile monster sits in the player's visible set,
--   accumulate one "safe turn" per call; on hitting 'regenInterval'
--   add 1 HP (capped at 'sMaxHP') and reset. A hostile becoming
--   visible immediately resets the counter.
--
--   Early-outs: full HP, dead, or victorious → no regen.
tickRegen :: RegenContext -> (Stats, Int)
tickRegen rc
  | rcDead rc    = (stats, counter)
  | rcVictory rc = (stats, counter)
  | sHP stats >= sMaxHP stats = (stats, 0)
  | hostileVisible            = (stats, 0)
  | otherwise =
      let next = counter + 1
      in if next >= regenInterval
           then (stats { sHP = min (sMaxHP stats) (sHP stats + 1) }, 0)
           else (stats, next)
  where
    stats    = rcStats rc
    counter  = rcCounter rc
    vis      = rcVisible rc
    monsters = rcMonsters rc
    hostileVisible = any (\m -> any (`Set.member` vis) (monsterTiles m)) monsters

-- | Advance the turn counter by one, unless dead or victory-frozen.
tickTurnCounter :: Bool -> Maybe Int -> Int -> Int
tickTurnCounter dead finalTurns turnsElapsed
  | dead              = turnsElapsed
  | Just _ <- finalTurns = turnsElapsed
  | otherwise         = turnsElapsed + 1
