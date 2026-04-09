{-# LANGUAGE OverloadedStrings #-}
-- | Canned-response mock for the AI backend.
--
--   Dispatches on keywords in the prompt to pick a plausible reply.
--   The goal is:
--
--   * tests can exercise the full AI pipeline without a network;
--   * a fresh install runs against a mock provider by default, so
--     the player never needs to bring up Ollama to experience the
--     AI-touched code paths;
--   * the mock replies are structured correctly for the downstream
--     parsers — the quest mock returns valid JSON, the greeting
--     mock returns plain text, etc.
--
--   Determinism matters: tests assert on specific replies, so the
--   dispatch logic is pure (wrapped in 'pure' at the boundary only
--   to match the real client's 'IO' signature).
module Game.AI.Mock
  ( mockReply
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

-- | Pick a canned reply for a prompt. The selection rule is a
--   simple keyword match: if the prompt talks about quests, return
--   a quest JSON blob; if it talks about greetings, return a line
--   of greeting text; otherwise return a generic room description.
--
--   'IO' in the signature is here only to match 'sendPrompt' — the
--   mock is deterministic and total.
mockReply :: Text -> IO Text
mockReply prompt
  | "quest" `T.isInfixOf` T.toLower prompt = pure mockQuestJSON
  | "greet" `T.isInfixOf` T.toLower prompt = pure mockGreeting
  | "room"  `T.isInfixOf` T.toLower prompt = pure mockRoomDesc
  | "describe a dungeon room" `T.isInfixOf` T.toLower prompt = pure mockRoomDesc
  | otherwise = pure mockGenericReply

-- | A valid JSON payload that 'Game.AI.QuestGen' can parse into a
--   real 'Game.Logic.Quest.Quest'. The fields match the schema
--   documented in 'Game.AI.Prompts.questPrompt'.
mockQuestJSON :: Text
mockQuestJSON = T.unlines
  [ "{"
  , "  \"name\": \"The Cave Hunt\","
  , "  \"description\": \"A veteran trapper has gone missing in the deeps. Thin their pursuers.\","
  , "  \"goal_type\": \"kill_monsters\","
  , "  \"goal_value\": 4,"
  , "  \"xp_reward\": 75"
  , "}"
  ]

-- | A short in-character NPC greeting. Deliberately atmospheric
--   rather than specific so it works for any NPC the game spawns.
mockGreeting :: Text
mockGreeting = "Another soul drawn to the dark? Mind your footing — the deep does not forgive."

-- | A one-sentence room description in the tone the real prompt asks
--   for. Used for the generic 'describe a room' path.
mockRoomDesc :: Text
mockRoomDesc = "Dust hangs in the still air; something old slept here, and its shape is still pressed into the stone."

-- | Catch-all for prompts that match none of the above keywords.
--   Returns a short acknowledgment so integration tests can tell the
--   mock from a silent failure.
mockGenericReply :: Text
mockGenericReply = "The mock oracle nods, but offers no answer."
