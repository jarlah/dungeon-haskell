{-# LANGUAGE OverloadedStrings #-}
module Game.AI.NPCBehaviourSpec (spec) where

import Test.Hspec
import qualified Data.Text as T

import Game.AI.Client (newAIClient, closeAIClient)
import Game.AI.NPCBehaviour (generateGreeting, cleanGreeting)
import Game.Config (AIConfig(..), AIProvider(..), defaultAIConfig)

mockCfg :: AIConfig
mockCfg = defaultAIConfig { aiEnabled = True, aiProvider = ProviderMock }

spec :: Spec
spec = do
  describe "cleanGreeting (pure)" $ do
    it "strips surrounding double quotes" $
      cleanGreeting "\"Hello, adventurer.\"" `shouldBe` "Hello, adventurer."

    it "strips surrounding single quotes" $
      cleanGreeting "'Hello, adventurer.'" `shouldBe` "Hello, adventurer."

    it "collapses internal newlines to spaces" $
      cleanGreeting "Hello,\nadventurer." `shouldBe` "Hello, adventurer."

    it "clamps to 240 characters" $ do
      let long = T.replicate 300 "x"
      T.length (cleanGreeting long) `shouldSatisfy` (<= 243) -- 240 + "..."

    it "strips whitespace" $
      cleanGreeting "  Hello.  " `shouldBe` "Hello."

    it "is a no-op on clean text" $
      cleanGreeting "Hello." `shouldBe` "Hello."

  describe "generateGreeting (mock integration)" $ do
    it "returns a non-empty greeting via mock provider" $ do
      client <- newAIClient mockCfg
      result <- generateGreeting client mockCfg "Quest Master" "Quest Master" 1
      closeAIClient client
      case result of
        Just txt -> T.length txt `shouldSatisfy` (> 0)
        Nothing  -> expectationFailure "expected a greeting from mock"

    it "returns Nothing when AI is disabled" $ do
      let cfg = defaultAIConfig { aiEnabled = False }
      client <- newAIClient cfg
      result <- generateGreeting client cfg "Test" "Test" 1
      closeAIClient client
      result `shouldBe` Nothing
