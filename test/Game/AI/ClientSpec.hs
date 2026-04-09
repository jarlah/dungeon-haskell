{-# LANGUAGE OverloadedStrings #-}
module Game.AI.ClientSpec (spec) where

import           Test.Hspec

import qualified Data.Text       as T

import           Game.AI.Client  (closeAIClient, newAIClient, sendPrompt)
import           Game.AI.Types   (AIError (..))
import           Game.Config     (AIProvider (..), defaultAIConfig, AIConfig (..))

-- | An AI config with the mock provider wired up — lets us exercise
--   the full sendPrompt path without a real network stack.
mockCfg :: AIConfig
mockCfg = defaultAIConfig { aiEnabled = True, aiProvider = ProviderMock }

disabledCfg :: AIConfig
disabledCfg = defaultAIConfig { aiEnabled = False }

spec :: Spec
spec = describe "Game.AI.Client" $ do

  describe "disabled path" $ do
    it "returns AIDisabled for any prompt when aiEnabled = False" $ do
      client <- newAIClient disabledCfg
      r <- sendPrompt client disabledCfg "anything"
      closeAIClient client
      r `shouldBe` Left AIDisabled

  describe "mock path" $ do
    it "returns the mock quest JSON for a quest prompt" $ do
      client <- newAIClient mockCfg
      r <- sendPrompt client mockCfg "Please generate a quest."
      closeAIClient client
      case r of
        Right txt -> txt `shouldSatisfy` ("\"goal_type\"" `T.isInfixOf`)
        Left err  -> expectationFailure (show err)

    it "returns a greeting for a greeting prompt" $ do
      client <- newAIClient mockCfg
      r <- sendPrompt client mockCfg "Please greet the adventurer."
      closeAIClient client
      case r of
        Right txt -> txt `shouldSatisfy` (not . T.null)
        Left err  -> expectationFailure (show err)

    it "closeAIClient is safe to call on a mock client" $ do
      client <- newAIClient mockCfg
      closeAIClient client
      -- if we got here without blowing up, we're good
      True `shouldBe` True
