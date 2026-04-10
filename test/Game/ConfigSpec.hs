{-# LANGUAGE OverloadedStrings #-}
module Game.ConfigSpec (spec) where

import           Test.Hspec

import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml             as Yaml
import           System.FilePath       ((</>))
import           System.IO             (hClose)
import           System.IO.Temp        (withSystemTempDirectory, withSystemTempFile)

import           Game.Config

spec :: Spec
spec = describe "Game.Config" $ do

  describe "defaultGameConfig" $ do
    it "starts with AI disabled and mock provider" $ do
      aiEnabled  (gcAI defaultGameConfig) `shouldBe` False
      aiProvider (gcAI defaultGameConfig) `shouldBe` ProviderMock

    it "has audio volumes in the 0..1 range" $ do
      let a = gcAudio defaultGameConfig
      acMusicVolume a `shouldSatisfy` (\v -> v >= 0 && v <= 1)
      acSfxVolume   a `shouldSatisfy` (\v -> v >= 0 && v <= 1)

  describe "FromJSON GameConfig" $ do
    it "accepts an empty document and returns defaults" $ do
      case Yaml.decodeEither' "{}" :: Either Yaml.ParseException GameConfig of
        Right cfg -> cfg `shouldBe` defaultGameConfig
        Left err  -> expectationFailure (show err)

    it "accepts a partial config and fills the rest from defaults" $ do
      let yaml = BS.pack $ unlines
            [ "ai:"
            , "  enabled: true"
            , "  provider: ollama"
            ]
      case Yaml.decodeEither' yaml :: Either Yaml.ParseException GameConfig of
        Right cfg -> do
          aiEnabled  (gcAI cfg) `shouldBe` True
          aiProvider (gcAI cfg) `shouldBe` ProviderOllama
          -- unmentioned fields still come from the default
          aiModel    (gcAI cfg) `shouldBe` aiModel defaultAIConfig
          -- audio wasn't mentioned at all, so the whole subsection is the default
          gcAudio cfg `shouldBe` defaultAudioConfig
        Left err -> expectationFailure (show err)

    it "parses all provider strings case-insensitively" $ do
      let parseProvider s = case Yaml.decodeEither' (BS.pack ("ai:\n  provider: " <> s)) :: Either Yaml.ParseException GameConfig of
            Right c -> Right (aiProvider (gcAI c))
            Left e  -> Left (show e)
      parseProvider "mock"      `shouldBe` Right ProviderMock
      parseProvider "Ollama"    `shouldBe` Right ProviderOllama
      parseProvider "OPENAI"    `shouldBe` Right ProviderOpenAI
      parseProvider "anthropic" `shouldBe` Right ProviderAnthropic
      parseProvider "ANTHROPIC" `shouldBe` Right ProviderAnthropic

    it "rejects an unknown provider with a helpful error" $ do
      let yaml = "ai:\n  provider: gpt5omega\n"
      case Yaml.decodeEither' (BS.pack yaml) :: Either Yaml.ParseException GameConfig of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected parse failure for unknown provider"

  describe "loadConfigFrom" $ do
    it "returns defaults (merged) for an empty file" $ do
      withSystemTempFile "cfg.yaml" $ \path h -> do
        hClose h
        writeFile path "{}\n"
        result <- loadConfigFrom path
        result `shouldBe` Right defaultGameConfig

    it "returns a Left when the file does not exist" $ do
      result <- loadConfigFrom "/nonexistent/path/config.yaml"
      case result of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected IO failure"

    it "reports a parse error with a readable message" $ do
      withSystemTempFile "bad.yaml" $ \path h -> do
        hClose h
        writeFile path "ai:\n  enabled: not-a-bool\n"
        result <- loadConfigFrom path
        case result of
          Left msg -> msg `shouldContain` "Config parse error"
          Right _  -> expectationFailure "expected parse failure"

  describe "resolveConfigPath" $ do
    it "prefers the CLI path when it exists" $
      withSystemTempDirectory "dhcfg" $ \dir -> do
        let p = dir </> "mine.yaml"
        writeFile p "{}\n"
        result <- resolveConfigPath (Just p)
        result `shouldBe` Just p

    it "returns Nothing when the CLI path does not exist" $ do
      result <- resolveConfigPath (Just "/nope/nope/nope.yaml")
      result `shouldBe` Nothing
