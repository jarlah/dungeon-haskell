{-# LANGUAGE ScopedTypeVariables #-}
module Game.Logic.QuestSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Game.Logic.Quest

-- | Generator for small, realistic quest goals. We pick small
--   targets so the property loops aren't pathologically deep.
instance Arbitrary QuestGoal where
  arbitrary = oneof
    [ GoalKillMonsters <$> choose (1, 10)
    , GoalReachDepth   <$> choose (1, 10)
    ]

instance Arbitrary QuestEvent where
  arbitrary = oneof
    [ pure EvKilledMonster
    , EvEnteredDepth <$> choose (1, 12)
    ]

-- | Build a fresh quest for property tests.
fresh :: QuestGoal -> Quest
fresh = mkQuest "t"

spec :: Spec
spec = describe "Game.Logic.Quest" $ do

  describe "mkQuest" $ do
    it "starts at zero progress and QuestActive" $ do
      let q = mkQuest "Slayer" (GoalKillMonsters 5)
      qProgress q `shouldBe` 0
      qStatus   q `shouldBe` QuestActive

  describe "advanceQuest" $ do
    it "advances a kill quest by one per EvKilledMonster" $ do
      let q0 = fresh (GoalKillMonsters 3)
          q1 = advanceQuest EvKilledMonster q0
          q2 = advanceQuest EvKilledMonster q1
      qProgress q1 `shouldBe` 1
      qProgress q2 `shouldBe` 2
      qStatus   q2 `shouldBe` QuestActive

    it "completes a kill quest when the target is reached" $ do
      let q0 = fresh (GoalKillMonsters 2)
          q1 = advanceQuest EvKilledMonster q0
          q2 = advanceQuest EvKilledMonster q1
      isCompleted q2 `shouldBe` True

    it "ignores depth events on a kill quest" $ do
      let q0 = fresh (GoalKillMonsters 3)
          q1 = advanceQuest (EvEnteredDepth 5) q0
      q1 `shouldBe` q0

    it "tracks max depth on a depth quest (not a running sum)" $ do
      let q0 = fresh (GoalReachDepth 5)
          q1 = advanceQuest (EvEnteredDepth 2) q0
          q2 = advanceQuest (EvEnteredDepth 1) q1  -- backwards: ignored
          q3 = advanceQuest (EvEnteredDepth 4) q2
      qProgress q3 `shouldBe` 4
      qStatus   q3 `shouldBe` QuestActive

    it "completes a depth quest on reaching the target" $ do
      let q0 = fresh (GoalReachDepth 3)
          q1 = advanceQuest (EvEnteredDepth 3) q0
      isCompleted q1 `shouldBe` True

    it "overshooting a depth quest still completes without going backwards" $ do
      let q0 = fresh (GoalReachDepth 3)
          q1 = advanceQuest (EvEnteredDepth 7) q0
      isCompleted q1 `shouldBe` True
      qProgress   q1 `shouldBe` 7

  describe "advanceAll" $ do
    it "applies the same event to every quest in a list" $ do
      let qs  = [ fresh (GoalKillMonsters 1)
                , fresh (GoalReachDepth 1)
                ]
          qs' = advanceAll EvKilledMonster qs
      map isCompleted qs' `shouldBe` [True, False]

  describe "terminal absorption" $ do
    it "prop: a completed quest stays completed under any further event" $
      property $ \(goal :: QuestGoal) (ev :: QuestEvent) ->
        let q0  = forceComplete (fresh goal)
            q1  = advanceQuest ev q0
        in qStatus q1 == QuestCompleted

    it "prop: a failed quest stays failed under any further event" $
      property $ \(goal :: QuestGoal) (ev :: QuestEvent) ->
        let q0 = (fresh goal) { qStatus = QuestFailed }
            q1 = advanceQuest ev q0
        in qStatus q1 == QuestFailed

  describe "monotonicity" $ do
    it "prop: progress never decreases after an event" $
      property $ \(goal :: QuestGoal) (ev :: QuestEvent) ->
        let q0 = fresh goal
            q1 = advanceQuest ev q0
        in qProgress q1 >= qProgress q0

    it "prop: kill-goal progress increases only under EvKilledMonster" $
      property $ \n d ->
        n > 0 ==>
        let q0 = fresh (GoalKillMonsters n)
            q1 = advanceQuest (EvEnteredDepth d) q0
        in qProgress q1 == qProgress q0

-- | Fire events until the quest completes. Assumes the goal is
--   reachable (which the generator guarantees — kill / depth
--   targets are ≥ 1).
forceComplete :: Quest -> Quest
forceComplete q = case qGoal q of
  GoalKillMonsters n ->
    iterate (advanceQuest EvKilledMonster) q !! n
  GoalReachDepth   n ->
    advanceQuest (EvEnteredDepth n) q
