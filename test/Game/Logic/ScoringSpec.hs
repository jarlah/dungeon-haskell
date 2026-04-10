module Game.Logic.ScoringSpec (spec) where

import Test.Hspec
import Game.Logic.Scoring (runRank)

spec :: Spec
spec = describe "Game.Logic.Scoring.runRank" $ do
  it "returns 'In Progress' for a live run with no victory" $
    runRank Nothing False 0 0 `shouldBe` "In Progress"

  it "returns 'Fallen' for a dead player" $
    runRank Nothing True 0 0 `shouldBe` "Fallen"

  it "returns 'Legendary' for <= 1500 turns, <= 3 potions, 0 saves" $
    runRank (Just 1500) False 3 0 `shouldBe` "Legendary"

  it "returns 'Heroic' for <= 2500 turns, <= 6 potions, <= 2 saves" $
    runRank (Just 2500) False 6 2 `shouldBe` "Heroic"

  it "returns 'Victor' for a victory that doesn't meet Heroic thresholds" $
    runRank (Just 3000) False 10 5 `shouldBe` "Victor"

  it "Legendary requires exactly 0 saves" $
    runRank (Just 1000) False 0 1 `shouldBe` "Heroic"

  it "Heroic boundary: 2501 turns drops to Victor" $
    runRank (Just 2501) False 0 0 `shouldBe` "Victor"

  it "4 potions disqualifies Legendary but qualifies Heroic" $
    runRank (Just 1000) False 4 0 `shouldBe` "Heroic"

  it "7 potions disqualifies Heroic" $
    runRank (Just 1000) False 7 0 `shouldBe` "Victor"
