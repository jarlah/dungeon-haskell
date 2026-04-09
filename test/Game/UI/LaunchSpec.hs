-- | Tests for the pure helpers in "Game.UI.Launch". The IO-side of
--   the launch flow (@loadMostRecent@, @runLaunchOption@) talks to
--   the save system and Brick's halt, so this spec only covers the
--   two pure pieces: cursor arithmetic and option resolution.
module Game.UI.LaunchSpec (spec) where

import Test.Hspec

import Game.GameState
  ( LaunchMenu (..), LaunchOption (..), launchOptions )
import Game.UI.Launch (resolveLaunchOption, stepLaunchCursor)

mkMenu :: Int -> Bool -> LaunchMenu
mkMenu c hasSaves = LaunchMenu { lmCursor = c, lmHasSaves = hasSaves }

spec :: Spec
spec = do

  describe "stepLaunchCursor" $ do

    it "moves the cursor by the given delta when in range" $ do
      lmCursor (stepLaunchCursor 1    (mkMenu 0 True)) `shouldBe` 1
      lmCursor (stepLaunchCursor 1    (mkMenu 1 True)) `shouldBe` 2

    it "wraps around at the top when stepping up" $
      lmCursor (stepLaunchCursor (-1) (mkMenu 0 True))
        `shouldBe` length launchOptions - 1

    it "wraps around at the bottom when stepping down" $
      lmCursor (stepLaunchCursor 1    (mkMenu (length launchOptions - 1) True))
        `shouldBe` 0

    it "leaves lmHasSaves untouched" $
      lmHasSaves (stepLaunchCursor 1 (mkMenu 0 False)) `shouldBe` False

  describe "resolveLaunchOption" $ do

    it "resolves the New Game cursor even without saves" $
      resolveLaunchOption (mkMenu 0 False) `shouldBe` Just LaunchNewGame

    it "resolves Continue when saves are present" $
      resolveLaunchOption (mkMenu 1 True) `shouldBe` Just LaunchContinue

    it "refuses Continue when there are no saves" $
      resolveLaunchOption (mkMenu 1 False) `shouldBe` Nothing

    it "resolves Load when saves are present" $
      resolveLaunchOption (mkMenu 2 True) `shouldBe` Just LaunchLoad

    it "refuses Load when there are no saves" $
      resolveLaunchOption (mkMenu 2 False) `shouldBe` Nothing

    it "resolves Quit regardless of saves" $ do
      resolveLaunchOption (mkMenu 3 True)  `shouldBe` Just LaunchQuit
      resolveLaunchOption (mkMenu 3 False) `shouldBe` Just LaunchQuit

    it "returns Nothing for an out-of-range cursor" $
      resolveLaunchOption (mkMenu 99 True) `shouldBe` Nothing
