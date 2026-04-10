module Game.Logic.ConstantsSpec (spec) where

import Test.Hspec

import Game.Types (Stats(..))
import Game.Logic.Constants

spec :: Spec
spec = do
  describe "Game.Logic.Constants" $ do
    it "fovRadius is positive" $
      fovRadius `shouldSatisfy` (> 0)

    it "monsterSightRadius is positive" $
      monsterSightRadius `shouldSatisfy` (> 0)

    it "regenInterval is positive" $
      regenInterval `shouldSatisfy` (> 0)

    it "dashMaxSteps is positive" $
      dashMaxSteps `shouldSatisfy` (> 0)

    it "dashCooldownTurns is positive" $
      dashCooldownTurns `shouldSatisfy` (> 0)

    it "defaultPlayerStats has HP equal to maxHP" $
      sHP defaultPlayerStats `shouldBe` sMaxHP defaultPlayerStats

    it "defaultPlayerStats starts at level 1" $
      sLevel defaultPlayerStats `shouldBe` 1

    it "launchOptions is non-empty" $
      launchOptions `shouldSatisfy` (not . null)
