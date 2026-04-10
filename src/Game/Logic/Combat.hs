module Game.Logic.Combat
  ( Damage(..)
  , CombatResult(..)
  , resolveWith
  , resolveAttack
  , applyDamage
  , isDead
  , resultDamage
  , describeAttack
  , describeAttacked
  , applyHitResult
  , monsterCombatEvent
  ) where

import System.Random (StdGen, randomR)

import Game.Types (Stats(..), Monster (..), GameEvent (..), isBoss, monsterName, itemName)
import Game.State.Types (GameState(..), emit)
import qualified Game.Logic.Progression as P
import qualified Game.Logic.Loot as Loot
import Game.Logic.Quest (QuestEvent(..), fireQuestEvent)
import Game.Utils.List (updateAt, removeAt)

newtype Damage = Damage { unDamage :: Int }
  deriving (Eq, Ord, Show)

data CombatResult
  = Miss
  | Hit         Damage
  | CriticalHit Damage
  | Kill        Damage
  deriving (Eq, Show)

-- | Pure attack resolution given two pre-rolled dice.
--
--   @roll@ is a d20 for to-hit; @critRoll@ is a d100 where <=10 crits.
--   All of combat's actual rules live here so they can be tested
--   deterministically without threading 'StdGen'.
resolveWith :: Int -> Int -> Stats -> Stats -> CombatResult
resolveWith roll critRoll atk def
  | isDead atk          = Miss
  | roll < hitThreshold = Miss
  | otherwise           =
      let base    = max 1 (sAttack atk - sDefense def `div` 2)
          critted = critRoll <= 10
          d       = Damage (if critted then base * 2 else base)
      in if sHP def - unDamage d <= 0
           then Kill        d
           else if critted
                  then CriticalHit d
                  else Hit         d
  where
    hitThreshold = max 5 (10 + sDefense def - sAttack atk)

-- | RNG-threading wrapper around 'resolveWith'.
resolveAttack :: StdGen -> Stats -> Stats -> (CombatResult, StdGen)
resolveAttack gen0 atk def =
  let (roll,     gen1) = randomR (1 :: Int, 20)  gen0
      (critRoll, gen2) = randomR (1 :: Int, 100) gen1
  in (resolveWith roll critRoll atk def, gen2)

applyDamage :: Stats -> Damage -> Stats
applyDamage s (Damage d) = s { sHP = max 0 (sHP s - d) }

isDead :: Stats -> Bool
isDead s = sHP s <= 0

resultDamage :: CombatResult -> Int
resultDamage Miss                     = 0
resultDamage (Hit         (Damage d)) = d
resultDamage (CriticalHit (Damage d)) = d
resultDamage (Kill        (Damage d)) = d

-- | Message for the player attacking a named target.
describeAttack :: CombatResult -> String -> String
describeAttack Miss                     target = "You miss the " ++ target ++ "."
describeAttack (Hit         (Damage d)) target = "You hit the " ++ target ++ " for " ++ show d ++ "."
describeAttack (CriticalHit (Damage d)) target = "You crit the " ++ target ++ " for " ++ show d ++ "!"
describeAttack (Kill        (Damage d)) target = "You kill the " ++ target ++ " (" ++ show d ++ ")."

-- | Message for a named attacker hitting the player.
describeAttacked :: CombatResult -> String -> String
describeAttacked Miss                     attacker = "The " ++ attacker ++ " misses you."
describeAttacked (Hit         (Damage d)) attacker = "The " ++ attacker ++ " hits you for " ++ show d ++ "."
describeAttacked (CriticalHit (Damage d)) attacker = "The " ++ attacker ++ " crits you for " ++ show d ++ "!"
describeAttacked (Kill        (Damage d)) attacker = "The " ++ attacker ++ " kills you (" ++ show d ++ ")."

-- | Shared post-resolution pipeline for any player-initiated hit
--   on a monster, whether from melee ('playerAttack') or ranged
--   ('Ranged.fireArrow'). Given:
--
--     * the game state with the /already-advanced/ RNG (the caller
--       is responsible for threading 'gsRng' through its own dice
--       rolls before handing us the state),
--     * the monster's index in 'gsMonsters' at the moment of
--       resolution,
--     * the monster record itself (used for kind, position, name),
--     * the 'C.CombatResult' from 'C.resolveAttack' / 'C.resolveWith',
--     * zero or more caller-supplied message lines to prepend under
--       the loot/level-up messages (the combat-line itself is
--       typically the only entry),
--
--   this helper:
--     * applies the damage to the monster,
--     * removes or updates the monster entry,
--     * rolls loot if the blow was fatal,
--     * awards XP and synthesises level-up messages,
--     * emits the right combat events ('EvAttackHit' / 'EvAttackCrit'
--       / 'EvMonsterKilled' / 'EvBossKilled'),
--     * freezes the run clock into 'gsFinalTurns' and flips
--       'gsVictory' on a boss kill,
--     * fires quest events ('EvKilledMonster' / 'EvKilledBoss')
--       so quests progress from either code path.
--
--   Centralising this means ranged kills win the game the same way
--   melee kills do, without the boss-victory snapshot being
--   accidentally duplicated or forgotten.
applyHitResult
  :: GameState
  -> Int
  -> Monster
  -> CombatResult
  -> [String]
  -> GameState
applyHitResult gs i m result hitMsgs =
  let newMStats      = applyDamage (mStats m) (Damage (resultDamage result))
      killed         = isDead newMStats
      wasBoss        = isBoss (mKind m)
      combatEv       = playerCombatEvent result
      (playerStats', levelMsgs, levelEvs) =
        if killed
          then
            let reward     = P.xpReward (mKind m)
                (s', ups)  = P.gainXP (gsPlayerStats gs) reward
                startLevel = sLevel (gsPlayerStats gs)
                endLevel   = sLevel s'
                msgs = [ "You reach level " ++ show l ++ "!"
                       | l <- [endLevel, endLevel - 1 .. startLevel + 1]
                       ]
                evs  = replicate ups EvLevelUp
            in (s', msgs, evs)
          else (gsPlayerStats gs, [], [])
      monsters' =
        if killed
          then removeAt i (gsMonsters gs)
          else updateAt i (\mo -> mo { mStats = newMStats }) (gsMonsters gs)
      -- Roll loot drops at the monster's tile if the blow was fatal.
      (loot, gen'') =
        if killed
          then Loot.rollLoot (gsRng gs) (mKind m)
          else ([], gsRng gs)
      lootMsgs =
        [ "The " ++ monsterName (mKind m) ++ " drops a " ++ itemName it ++ "."
        | it <- loot
        ]
      itemsOnFloor' =
        gsItemsOnFloor gs ++ [ (mPos m, it) | it <- loot ]
      bossEvs  = [ EvBossKilled | killed && wasBoss ]
      bossMsgs = [ "With a final roar, the " ++ monsterName (mKind m)
                   ++ " falls. You are victorious!"
                 | killed && wasBoss ]
      victory' = gsVictory gs || (killed && wasBoss)
      finalTurns' = case gsFinalTurns gs of
        Just _  -> gsFinalTurns gs
        Nothing
          | victory' && not (gsVictory gs) -> Just (gsTurnsElapsed gs)
          | otherwise                      -> Nothing
      gs' = emit
        gs
          { gsMonsters     = monsters'
          , gsPlayerStats  = playerStats'
          , gsRng          = gen''
          , gsMessages     =
              reverse lootMsgs ++ bossMsgs ++ levelMsgs
                ++ reverse hitMsgs ++ gsMessages gs
          , gsItemsOnFloor = itemsOnFloor'
          , gsVictory      = victory'
          , gsFinalTurns   = finalTurns'
          }
        (combatEv : levelEvs ++ bossEvs)
      questEvs = if wasBoss then [EvKilledMonster, EvKilledBoss]
                            else [EvKilledMonster]
      applyQuestEv ev s =
        let (qs, ms) = fireQuestEvent ev (gsQuests s)
        in s { gsQuests = qs, gsMessages = reverse ms ++ gsMessages s }
      fireAll gss = foldl (flip applyQuestEv) gss questEvs
  in if killed then fireAll gs' else gs'

-- | Map a combat result to the event the *attacker* cares about
--   when the attacker is the player.
playerCombatEvent :: CombatResult -> GameEvent
playerCombatEvent Miss            = EvAttackMiss
playerCombatEvent (Hit _)         = EvAttackHit
playerCombatEvent (CriticalHit _) = EvAttackCrit
playerCombatEvent (Kill _)        = EvMonsterKilled

-- | Map a combat result to the event for the player being hit.
--   'Nothing' means "no sound for this" (we skip monster whiffs).
monsterCombatEvent :: CombatResult -> Maybe GameEvent
monsterCombatEvent Miss            = Nothing
monsterCombatEvent (Hit _)         = Just EvPlayerHurt
monsterCombatEvent (CriticalHit _) = Just EvPlayerHurt
monsterCombatEvent (Kill _)        = Just EvPlayerDied