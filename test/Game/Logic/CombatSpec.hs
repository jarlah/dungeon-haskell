{-# LANGUAGE ScopedTypeVariables #-}
module Game.Logic.CombatSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Game.Logic.Combat
import Game.Types (Stats(..))

-- | A stats generator that produces "living" combatants with
--   non-negative numbers and HP in [1, maxHP].
genLivingStats :: Gen Stats
genLivingStats = do
  maxHp <- choose (1, 100)
  hp    <- choose (1, maxHp)
  atk   <- choose (0, 30)
  dfn   <- choose (0, 30)
  spd   <- choose (1, 20)
  pure (Stats hp maxHp atk dfn spd)

-- | Same as above but HP may be 0 (dead).
genAnyStats :: Gen Stats
genAnyStats = do
  maxHp <- choose (1, 100)
  hp    <- choose (0, maxHp)
  atk   <- choose (0, 30)
  dfn   <- choose (0, 30)
  spd   <- choose (1, 20)
  pure (Stats hp maxHp atk dfn spd)

spec :: Spec
spec = describe "Game.Logic.Combat" $ do

  it "prop_damageNeverNegative" $ property $
    forAll (choose (1 :: Int, 20))  $ \roll ->
    forAll (choose (1 :: Int, 100)) $ \crit ->
    forAll genLivingStats $ \atk ->
    forAll genLivingStats $ \dfn ->
      resultDamage (resolveWith roll crit atk dfn) >= 0

  it "prop_armorReducesDamage" $ property $
    forAll (choose (1 :: Int, 20))  $ \roll ->
    forAll (choose (11 :: Int, 100)) $ \crit ->  -- no crit, keep it deterministic
    forAll genLivingStats $ \atk ->
    forAll genLivingStats $ \dfn ->
      let unarmored = dfn { sDefense = 0 }
          armored   = dfn { sDefense = 10 }
          dmgUn     = resultDamage (resolveWith roll crit atk unarmored)
          dmgAr     = resultDamage (resolveWith roll crit atk armored)
      in dmgAr <= dmgUn

  it "prop_zeroHPMeansDead" $ property $
    forAll genAnyStats $ \s ->
      isDead (s { sHP = 0 })

  it "prop_deadCannotAttack" $ property $
    forAll (choose (1 :: Int, 20))  $ \roll ->
    forAll (choose (1 :: Int, 100)) $ \crit ->
    forAll genLivingStats $ \atk ->
    forAll genLivingStats $ \dfn ->
      let deadAtk = atk { sHP = 0 }
      in resolveWith roll crit deadAtk dfn === Miss

  it "prop_criticalHitDoesMoreDamage" $ property $
    forAll (choose (1 :: Int, 20)) $ \roll ->
    forAll genLivingStats $ \atk ->
    forAll genLivingStats $ \dfn' ->
      -- Make the defender very tanky so the result is Hit/CriticalHit,
      -- not Kill (Kill would mask the crit multiplier in both branches).
      let dfn  = dfn' { sHP = 10000, sMaxHP = 10000 }
          nonCrit = resultDamage (resolveWith roll 100 atk dfn)
          crit    = resultDamage (resolveWith roll 1   atk dfn)
      in nonCrit > 0 ==> crit >= nonCrit

  it "prop_applyDamageNeverBelowZero" $ property $
    forAll genAnyStats $ \s ->
    forAll (choose (0 :: Int, 1000)) $ \d ->
      sHP (applyDamage s (Damage d)) >= 0

  it "prop_higherDefenseBlocksMoreAttacks" $ property $
    forAll genLivingStats $ \atk ->
    forAll genLivingStats $ \dfn ->
      let lowDef  = dfn { sDefense = 0 }
          highDef = dfn { sDefense = 20 }
          hitCount d =
            length [ () | roll <- [1 .. 20]
                        , resolveWith roll 100 atk d /= Miss ]
      in hitCount highDef <= hitCount lowDef
