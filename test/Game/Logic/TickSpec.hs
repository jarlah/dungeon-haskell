module Game.Logic.TickSpec (spec) where

import Test.Hspec
import qualified Data.Set as Set

import Game.Types (Stats(..), Monster(..), MonsterKind(..), mkMonster)
import Game.Logic.Tick (tickDash, tickRegen, tickTurnCounter)
import Game.Logic.Constants (regenInterval)
import Linear (V2(..))

baseStats :: Stats
baseStats = Stats
  { sHP = 20, sMaxHP = 25, sAttack = 6, sDefense = 2
  , sSpeed = 10, sLevel = 1, sXP = 0
  }

spec :: Spec
spec = do
  describe "tickDash" $ do
    it "decrements a positive cooldown by one" $
      tickDash 5 `shouldBe` 4

    it "leaves zero unchanged" $
      tickDash 0 `shouldBe` 0

    it "decrements 1 to 0" $
      tickDash 1 `shouldBe` 0

  describe "tickRegen" $ do
    let noMonsters = [] :: [Monster]
        emptyVis   = Set.empty

    it "is a no-op when dead" $ do
      let (stats', counter') = tickRegen True False baseStats 5 emptyVis noMonsters
      stats' `shouldBe` baseStats
      counter' `shouldBe` 5

    it "is a no-op when victorious" $ do
      let (stats', counter') = tickRegen False True baseStats 5 emptyVis noMonsters
      stats' `shouldBe` baseStats
      counter' `shouldBe` 5

    it "resets counter to 0 at full HP" $ do
      let fullStats = baseStats { sHP = 25 }
          (stats', counter') = tickRegen False False fullStats 8 emptyVis noMonsters
      stats' `shouldBe` fullStats
      counter' `shouldBe` 0

    it "resets counter when a hostile is visible" $ do
      let rat = mkMonster Rat (V2 2 2)
          vis = Set.singleton (V2 2 2)
          (stats', counter') = tickRegen False False baseStats 10 vis [rat]
      stats' `shouldBe` baseStats
      counter' `shouldBe` 0

    it "increments counter when safe and below full HP" $ do
      let (_, counter') = tickRegen False False baseStats 0 emptyVis noMonsters
      counter' `shouldBe` 1

    it "heals 1 HP and resets counter at regenInterval" $ do
      let (stats', counter') = tickRegen False False baseStats (regenInterval - 1) emptyVis noMonsters
      sHP stats' `shouldBe` sHP baseStats + 1
      counter' `shouldBe` 0

    it "never heals above sMaxHP" $ do
      let almostFull = baseStats { sHP = sMaxHP baseStats - 1 }
          (stats', _) = tickRegen False False almostFull (regenInterval - 1) emptyVis noMonsters
      sHP stats' `shouldBe` sMaxHP baseStats

    it "does not tick when monster exists but is not visible" $ do
      let rat = mkMonster Rat (V2 2 2)
          vis = Set.singleton (V2 4 4)  -- monster not in visible set
          (_, counter') = tickRegen False False baseStats 5 vis [rat]
      counter' `shouldBe` 6

  describe "tickTurnCounter" $ do
    it "increments by one normally" $
      tickTurnCounter False Nothing 100 `shouldBe` 101

    it "freezes when dead" $
      tickTurnCounter True Nothing 100 `shouldBe` 100

    it "freezes when finalTurns is Just" $
      tickTurnCounter False (Just 50) 100 `shouldBe` 100
