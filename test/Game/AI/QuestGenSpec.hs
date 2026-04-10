{-# LANGUAGE OverloadedStrings #-}
module Game.AI.QuestGenSpec (spec) where

import           Test.Hspec

import qualified Data.Text        as T

import           Game.AI.Client   (newAIClient, closeAIClient)
import           Game.AI.QuestGen (generateQuest, parseQuestJSON)
import           Game.Config      (AIConfig(..), AIProvider(..), defaultAIConfig)
import           Game.Logic.Quest (Quest (..), QuestGoal (..), QuestStatus (..))

mockCfg :: AIConfig
mockCfg = defaultAIConfig { aiEnabled = True, aiProvider = ProviderMock }

spec :: Spec
spec = describe "Game.AI.QuestGen.parseQuestJSON" $ do

  it "parses a well-formed kill_monsters quest" $ do
    let raw = T.unlines
          [ "{"
          , "  \"name\": \"Rat Patrol\","
          , "  \"description\": \"Kill some rats.\","
          , "  \"goal_type\": \"kill_monsters\","
          , "  \"goal_value\": 5,"
          , "  \"xp_reward\": 50"
          , "}"
          ]
    case parseQuestJSON raw of
      Right q -> do
        qName   q `shouldBe` "Rat Patrol"
        qGoal   q `shouldBe` GoalKillMonsters 5
        qReward q `shouldBe` 50
        qStatus q `shouldBe` QuestNotStarted
      Left err -> expectationFailure err

  it "parses a reach_depth quest" $ do
    let raw = "{\"name\":\"Delve\",\"goal_type\":\"reach_depth\",\"goal_value\":7,\"xp_reward\":200}"
    case parseQuestJSON raw of
      Right q -> qGoal q `shouldBe` GoalReachDepth 7
      Left e  -> expectationFailure e

  it "parses a kill_boss quest without needing goal_value" $ do
    let raw = "{\"name\":\"Slay\",\"goal_type\":\"kill_boss\",\"xp_reward\":400}"
    case parseQuestJSON raw of
      Right q -> qGoal q `shouldBe` GoalKillBoss
      Left e  -> expectationFailure e

  it "clamps a runaway kill count to the allowed range" $ do
    let raw = "{\"name\":\"Massacre\",\"goal_type\":\"kill_monsters\",\"goal_value\":9999,\"xp_reward\":50}"
    case parseQuestJSON raw of
      Right q -> qGoal q `shouldBe` GoalKillMonsters 20
      Left e  -> expectationFailure e

  it "clamps a runaway XP reward" $ do
    let raw = "{\"name\":\"Rich\",\"goal_type\":\"kill_boss\",\"xp_reward\":1000000}"
    case parseQuestJSON raw of
      Right q -> qReward q `shouldBe` 500
      Left e  -> expectationFailure e

  it "accepts a ```json fenced reply" $ do
    let raw = T.unlines
          [ "```json"
          , "{\"name\":\"Fenced\",\"goal_type\":\"kill_boss\",\"xp_reward\":100}"
          , "```"
          ]
    case parseQuestJSON raw of
      Right q -> qName q `shouldBe` "Fenced"
      Left e  -> expectationFailure e

  it "rejects an unknown goal_type" $ do
    let raw = "{\"name\":\"Weird\",\"goal_type\":\"collect_cheese\",\"xp_reward\":10}"
    case parseQuestJSON raw of
      Left _  -> pure ()
      Right _ -> expectationFailure "expected failure for unknown goal_type"

  it "rejects malformed JSON" $ do
    case parseQuestJSON "not json at all" of
      Left _  -> pure ()
      Right _ -> expectationFailure "expected JSON parse failure"

  it "sanitizes a name with control characters" $ do
    let raw = "{\"name\":\"Hello\\u0001World\",\"goal_type\":\"kill_boss\",\"xp_reward\":10}"
    case parseQuestJSON raw of
      Right q -> qName q `shouldBe` "HelloWorld"
      Left e  -> expectationFailure e

  it "truncates a very long name" $ do
    let longName = replicate 200 'x'
        raw      = "{\"name\":\"" <> T.pack longName <> "\",\"goal_type\":\"kill_boss\",\"xp_reward\":10}"
    case parseQuestJSON raw of
      Right q -> length (qName q) `shouldSatisfy` (<= 40)
      Left e  -> expectationFailure e

  it "falls back to a placeholder for an empty name" $ do
    let raw = "{\"name\":\"\",\"goal_type\":\"kill_boss\",\"xp_reward\":10}"
    case parseQuestJSON raw of
      Right q -> qName q `shouldBe` "Unnamed Quest"
      Left e  -> expectationFailure e

  describe "generateQuest (mock integration)" $ do
    it "returns a valid quest via mock provider" $ do
      client <- newAIClient mockCfg
      result <- generateQuest client mockCfg 1 1 0
      closeAIClient client
      case result of
        Just q -> do
          qName q `shouldSatisfy` (not . null)
          qStatus q `shouldBe` QuestNotStarted
          qReward q `shouldSatisfy` (> 0)
        Nothing -> expectationFailure "expected a quest from mock"

    it "returns Nothing when AI is disabled" $ do
      let cfg = defaultAIConfig { aiEnabled = False }
      client <- newAIClient cfg
      result <- generateQuest client cfg 1 1 0
      closeAIClient client
      result `shouldBe` Nothing
