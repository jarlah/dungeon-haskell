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
  ) where

import System.Random (StdGen, randomR)

import Game.Types (Stats(..))

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
