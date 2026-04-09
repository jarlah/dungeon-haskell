{-# LANGUAGE OverloadedStrings #-}
module Game.AI.MockSpec (spec) where

import           Test.Hspec

import qualified Data.Text       as T

import           Game.AI.Mock    (mockReply)

spec :: Spec
spec = describe "Game.AI.Mock" $ do

  it "returns parseable quest JSON for a quest prompt" $ do
    reply <- mockReply "Generate a quest for the adventurer."
    reply `shouldSatisfy` ("\"name\"" `T.isInfixOf`)
    reply `shouldSatisfy` ("\"goal_type\"" `T.isInfixOf`)

  it "returns a greeting for a greeting prompt" $ do
    reply <- mockReply "Please greet the adventurer."
    reply `shouldSatisfy` (not . T.null)
    -- must not look like JSON
    reply `shouldNotSatisfy` ("{" `T.isPrefixOf`)

  it "returns a room description for a room prompt" $ do
    reply <- mockReply "Describe a dungeon room for the player."
    reply `shouldSatisfy` (not . T.null)
    reply `shouldNotSatisfy` ("{" `T.isPrefixOf`)

  it "falls back to a generic reply for an unrecognized prompt" $ do
    reply <- mockReply "What is the airspeed velocity of an unladen swallow?"
    reply `shouldSatisfy` (not . T.null)

  it "is case-insensitive on keyword dispatch" $ do
    lower <- mockReply "generate a quest"
    upper <- mockReply "GENERATE A QUEST"
    lower `shouldBe` upper
