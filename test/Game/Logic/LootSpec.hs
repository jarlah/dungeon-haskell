{-# LANGUAGE ScopedTypeVariables #-}
-- | Tests for 'Game.Logic.Loot'. Loot is driven by a seeded RNG,
--   so we can test structural properties over many seeds without
--   committing to any specific roll outcome.
module Game.Logic.LootSpec (spec) where

import System.Random (mkStdGen)
import Test.Hspec
import Test.QuickCheck

import Game.Logic.Loot
import Game.Types

genKind :: Gen MonsterKind
genKind = elements [minBound .. maxBound]

spec :: Spec
spec = describe "Game.Logic.Loot.rollLoot" $ do

  it "never drops more than one item per kill" $ property $
    \seed -> forAll genKind $ \k ->
      let (items, _) = rollLoot (mkStdGen seed) k
      in length items <= 1

  it "never drops an item not in the kind's table" $ property $
    \seed -> forAll genKind $ \k ->
      let (items, _) = rollLoot (mkStdGen seed) k
          allowed    = map snd (lootTable k)
      in all (`elem` allowed) items

  it "drop chance per kind is strictly ordered: Rat < Goblin < Orc" $ do
    dropChance Rat    < dropChance Goblin `shouldBe` True
    dropChance Goblin < dropChance Orc    `shouldBe` True

  it "rats mostly drop nothing over a large sample" $ do
    -- Sanity check: rat drop chance is 20%, so over 200 rolls we
    -- expect ~160 empty drops. Allow wide slack so the test is
    -- not flaky.
    let rolls   = [ fst (rollLoot (mkStdGen s) Rat) | s <- [1 .. 200] ]
        empties = length (filter null rolls)
    empties `shouldSatisfy` (>= 120)

  it "orcs drop something most of the time over a large sample" $ do
    -- Orc drop chance is 75%; expect ~150 drops out of 200.
    let rolls = [ fst (rollLoot (mkStdGen s) Orc) | s <- [1 .. 200] ]
        drops = length (filter (not . null) rolls)
    drops `shouldSatisfy` (>= 120)
