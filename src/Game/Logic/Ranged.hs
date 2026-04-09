-- | Pure helpers for ranged attacks. Everything here is either a
--   data description or a pure transform — there is no 'GameState'
--   anywhere. The glue that wires this module into the player-turn
--   pipeline lives in "Game.GameState" as 'Game.GameState.fireArrow';
--   that adapter builds the predicates 'resolveShot' needs out of
--   the live 'GameState' and interprets the 'ShotOutcome' we return
--   (decrementing the quiver, advancing the RNG, and handing
--   'ShotLanded' hits off to 'applyHitResult').
--
--   Exports:
--
--     * 'arrowRange' — the per-shot tile cap.
--     * 'RayOutcome' and 'walkRay' — the tile-by-tile raycast
--       that decides whether an arrow flies past, thuds into a
--       wall, is absorbed by an NPC or chest, or lands on a
--       monster.
--     * 'ShotOutcome' and 'resolveShot' — the full "what would
--       firing an arrow do right now?" resolver, including the
--       refusal/miss/hit classification, the to-hit and crit
--       rolls, and the pre-formatted flavor message.
--
--   Keeping this module free of 'GameState' lets us unit test
--   shots against hand-built fixtures and stops the import graph
--   from cycling back through "Game.GameState".
module Game.Logic.Ranged
  ( arrowRange
  , RayOutcome (..)
  , walkRay
  , ShotOutcome (..)
  , resolveShot
  ) where

import Game.Types
    ( Monster, DungeonLevel, Tile(..), DoorState(..), Pos, tileAt, Dir
    , Inventory(..), invWeapon, Stats(..)
    , dirToOffset, monsterName, mKind, mStats, Weapon (..)
    )
import qualified Game.Logic.Combat as C
import qualified Game.Logic.Inventory as Inv
import System.Random (StdGen, randomR)

-- | How many tiles an arrow travels before falling harmlessly.
--   Tuned to be comfortably more than a single corridor's worth
--   but well short of a whole-room diagonal so bow play feels
--   different from melee without trivializing approach tactics.
arrowRange :: Int
arrowRange = 8

-- | Possible outcomes of tracing an arrow's path. The string
--   carried by 'RayBlocked' is the tail of a message — the caller
--   stitches it onto "Your arrow " to form a full line.
data RayOutcome
  = RayHitMonster !Int !Monster
  | RayBlocked    !String
  | RayDropped
  deriving (Eq, Show)

-- | Description of what firing one arrow /would/ do, given a
--   fixed snapshot of the world. 'resolveShot' returns one of
--   these and the caller in "Game.GameState" interprets it — this
--   module never touches 'GameState' directly.
--
--   Exactly three things can happen to a shot:
data ShotOutcome
  -- | No arrow left the quiver. The player either has no bow
  --   equipped or has an empty quiver. The 'String' is the
  --   message to show. Callers must /not/ decrement arrows on
  --   this outcome, and the turn must not advance — refused
  --   shots are free no-ops, matching the "bump a wall" rule.
  = ShotRefused !String
  -- | An arrow was fired but didn't land on a monster — it was
  --   stopped by terrain (wall, closed or locked door), absorbed
  --   by an NPC or chest, or flew off the end of 'arrowRange'.
  --   The 'String' is a full "Your arrow …" message. Callers
  --   /must/ decrement arrows and advance the turn: the quiver
  --   has one fewer shot and the attempt cost a turn.
  | ShotMissed !String
  -- | An arrow struck a monster. Carries:
  --
  --     * the monster's index into the live monster list,
  --     * the 'Monster' record at the moment of resolution
  --       (used for kind, name, and stats),
  --     * the 'C.CombatResult' from 'C.resolveWith' — miss,
  --       hit, crit, or kill, plus the damage dealt,
  --     * the pre-formatted "Your arrow …" flavor line,
  --     * the advanced 'StdGen' /after/ both the to-hit and
  --       crit rolls have been drawn.
  --
  --   Callers must decrement arrows, write the carried 'StdGen'
  --   back into 'gsRng', and then hand the hit off to
  --   'applyHitResult' for damage, loot, XP, and quest events.
  | ShotLanded !Int !Monster !C.CombatResult !String !StdGen
  deriving (Eq, Show)

-- | Resolve one arrow shot as a pure, descriptive value. Given a
--   snapshot of the player, their equipment, and three predicates
--   summarising the live entity lists, return a 'ShotOutcome'
--   describing what /would/ happen if the player fired right now.
--
--   This function never touches 'GameState'. It does not
--   decrement arrows, does not advance the turn, and does not
--   apply damage — all of that is the caller's job. Returning a
--   'ShotOutcome' instead of a new 'GameState' is what keeps this
--   module testable against hand-built fixtures and free of any
--   import back into "Game.GameState".
--
--   Resolution rules:
--
--     * No bow equipped → 'ShotRefused' with a "no bow" message.
--       RNG and arrow count are untouched; the caller must leave
--       the turn clock alone.
--     * Empty quiver → 'ShotRefused' with a "no arrows" message.
--       Same no-turn-spent convention.
--     * Otherwise the ray is traced via 'walkRay' starting /one
--       tile ahead/ of the shooter (the shot never overlaps the
--       shooter's own square) and capped at 'arrowRange' tiles:
--
--         - 'RayBlocked' or 'RayDropped' become 'ShotMissed'
--           with a flavor message. The arrow is gone.
--         - 'RayHitMonster' layers 'Inv.bowRangedBonus' on top
--           of the player's 'sAttack', rolls a d20 to-hit and
--           a d100 crit via 'C.resolveWith', and returns
--           'ShotLanded' carrying the combat result, a
--           formatted message, and the 'StdGen' /after/ both
--           rolls have been drawn.
resolveShot
  :: Dir                              -- ^ direction of the shot
  -> DungeonLevel                     -- ^ dungeon for terrain lookups
  -> Pos                              -- ^ shooter's tile; ray starts one step past
  -> (Pos -> Maybe (Int, Monster))    -- ^ monster-at-tile lookup
  -> (Pos -> Bool)                    -- ^ NPC present at tile?
  -> (Pos -> Bool)                    -- ^ chest present at tile?
  -> Inventory                        -- ^ player inventory (bow + quiver check)
  -> Stats                            -- ^ player base stats
  -> StdGen                           -- ^ RNG for to-hit + crit rolls
  -> ShotOutcome
resolveShot dir level pos monsterL npcHit chestHit inv stats rng
  | invWeapon inv /= Just Bow = ShotRefused "You don't have a bow equipped."
  | invArrows inv <= 0        = ShotRefused "You have no arrows."
  | otherwise =
      let step = dirToOffset dir
          path = [ pos + fmap (* k) step | k <- [1 .. arrowRange] ]
      in case walkRay level monsterL npcHit chestHit path of
           RayBlocked tail_ -> ShotMissed ("Your arrow " ++ tail_ ++ ".")
           RayDropped       -> ShotMissed "Your arrow sails away and is lost."
           RayHitMonster i m ->
             let rangedAtk      = stats { sAttack = sAttack stats + Inv.bowRangedBonus }
                 (roll,    g1)  = randomR (1 :: Int, 20)  rng
                 (critRoll, g2) = randomR (1 :: Int, 100) g1
                 result         = C.resolveWith roll critRoll rangedAtk (mStats m)
                 verb = case result of
                   C.Miss          -> "glances off"
                   C.Hit _         -> "strikes"
                   C.CriticalHit _ -> "tears through"
                   C.Kill _        -> "fells"
                 msg  = "Your arrow " ++ verb ++ " the "
                     ++ monsterName (mKind m) ++ "."
             in ShotLanded i m result msg g2

-- | Walk a precomputed list of tiles in flight order and return
--   the first stopping event. Caller supplies:
--
--     * the dungeon level (for terrain),
--     * the monster list (and a monster-at-pos lookup),
--     * the NPC list (treated as arrow-stopping friendly fire),
--     * the chest list (treated as solid world objects),
--     * the precomputed path.
--
--   Taking each collection as a plain argument keeps this module
--   free of any 'GameState' dependency and lets the existing
--   unit test harness exercise it against hand-built fixtures.
walkRay
  :: DungeonLevel
  -> (Pos -> Maybe (Int, Monster))
  -> (Pos -> Bool)   -- ^ NPC present at position?
  -> (Pos -> Bool)   -- ^ chest present at position?
  -> [Pos]
  -> RayOutcome
walkRay _  _        _      _        [] = RayDropped
walkRay dl monsterL npcHit chestHit (p : ps) =
  case tileAt dl p of
    Nothing   -> RayBlocked "clatters off into the void"
    Just Wall -> RayBlocked "clatters against the wall"
    Just (Door Closed)     -> RayBlocked "thuds into the closed door"
    Just (Door (Locked _)) -> RayBlocked "thuds into the locked door"
    _
      | npcHit p   -> RayBlocked "whistles past a friendly face"
      | chestHit p -> RayBlocked "strikes a chest with a dull thunk"
      | otherwise  -> case monsterL p of
          Just (i, m) -> RayHitMonster i m
          Nothing     -> walkRay dl monsterL npcHit chestHit ps
