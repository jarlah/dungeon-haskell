{-# LANGUAGE OverloadedStrings #-}
module Game.AI.LevelContentSpec (spec) where

import Test.Hspec
import qualified Data.Text as T

import Game.AI.Client (newAIClient, closeAIClient)
import Game.AI.LevelContent (describeRoom, cleanDescription)
import Game.Logic.Dungeon (Room(..))
import Game.Config (AIConfig(..), AIProvider(..), defaultAIConfig)

mockCfg :: AIConfig
mockCfg = defaultAIConfig { aiEnabled = True, aiProvider = ProviderMock }

spec :: Spec
spec = do
  describe "cleanDescription (pure)" $ do
    it "strips surrounding double quotes" $
      cleanDescription "\"A dark room.\"" `shouldBe` "A dark room."

    it "collapses internal newlines to spaces" $
      cleanDescription "A dark\nroom." `shouldBe` "A dark room."

    it "clamps to 200 characters" $ do
      let long = T.replicate 250 "x"
      T.length (cleanDescription long) `shouldSatisfy` (<= 203) -- 200 + "..."

    it "strips whitespace" $
      cleanDescription "  A room.  " `shouldBe` "A room."

    it "is a no-op on clean text" $
      cleanDescription "A room." `shouldBe` "A room."

  describe "describeRoom (mock integration)" $ do
    it "returns a non-empty description via mock provider" $ do
      client <- newAIClient mockCfg
      let room = Room 1 1 5 5
      result <- describeRoom client mockCfg room 1 ["rat", "goblin"]
      closeAIClient client
      case result of
        Just txt -> T.length txt `shouldSatisfy` (> 0)
        Nothing  -> expectationFailure "expected a description from mock"

    it "returns Nothing when AI is disabled" $ do
      let cfg = defaultAIConfig { aiEnabled = False }
      client <- newAIClient cfg
      result <- describeRoom client cfg (Room 1 1 5 5) 1 []
      closeAIClient client
      result `shouldBe` Nothing
