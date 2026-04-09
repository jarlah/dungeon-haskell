{-# LANGUAGE OverloadedStrings #-}
-- | Prompt templates for the AI content-generation pipeline.
--
--   Every prompt is built by a pure function that takes whatever
--   structured inputs it needs and produces a 'Text'. No IO, no
--   network — this module is trivially testable and every prompt
--   change lands in one diffable place.
module Game.AI.Prompts
  ( questPrompt
  , greetingPrompt
  , roomDescPrompt
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Build the quest-generation prompt. The output is a JSON object
--   the LLM should return; the parser in "Game.AI.QuestGen" reads
--   that JSON into a 'Game.Logic.Quest.Quest'.
questPrompt
  :: Int      -- ^ current dungeon depth
  -> Int      -- ^ player level
  -> Int      -- ^ monsters killed so far
  -> Text
questPrompt depth level kills = T.unlines
  [ "You are a quest designer for a dungeon crawler."
  , "The player is on depth " <> tshow depth
      <> ", level " <> tshow level
      <> ", has killed " <> tshow kills <> " monsters."
  , "Generate a quest as a JSON object with exactly these keys:"
  , "  \"name\":        short title (max 40 chars)"
  , "  \"description\": 1-2 sentence flavor text"
  , "  \"goal_type\":   one of \"kill_monsters\" | \"reach_depth\" | \"kill_boss\""
  , "  \"goal_value\":  integer — for kill_monsters the count (1-20),"
  , "                  for reach_depth the target depth (2-11),"
  , "                  for kill_boss ignored (use 1)"
  , "  \"xp_reward\":   integer reward (25-500)"
  , ""
  , "Respond with ONLY the JSON object, no markdown fences, no prose."
  ]

-- | Build the NPC greeting prompt. The LLM is asked for a one-to-two
--   sentence in-character greeting, with no quoting or decoration.
greetingPrompt
  :: Text     -- ^ NPC name
  -> Text     -- ^ NPC role (e.g. "Quest Master", "Merchant")
  -> Int      -- ^ current dungeon depth
  -> Text
greetingPrompt name role depth = T.unlines
  [ "You are an NPC in a dark dungeon."
  , "Your name is " <> name <> "."
  , "You are a " <> role <> "."
  , "The adventurer approaches you on dungeon depth " <> tshow depth <> "."
  , "Greet them in 1-2 short sentences. Be atmospheric and in-character."
  , "Respond with ONLY the greeting text — no quotes, no prefix, no markdown."
  ]

-- | Build the room-description prompt for level content generation.
--   The LLM is asked for a single atmospheric sentence describing
--   the room based on its size, the current depth, and any monsters
--   already present.
roomDescPrompt
  :: Int        -- ^ room width in tiles
  -> Int        -- ^ room height in tiles
  -> Int        -- ^ current dungeon depth
  -> [Text]     -- ^ names of monsters in the room
  -> Text
roomDescPrompt w h depth monsters = T.unlines
  [ "Describe a dungeon room in 1 short atmospheric sentence."
  , "Room size: " <> tshow w <> "x" <> tshow h <> "."
  , "Depth: " <> tshow depth <> "."
  , "Monsters present: " <> monsterList
  , "Respond with ONLY the description — no quotes, no prefix."
  ]
  where
    monsterList
      | null monsters = "none"
      | otherwise     = T.intercalate ", " monsters

-- | Show an 'Int' as 'Text' without the @Text.Show@ overhead of
--   packing a 'String'.
tshow :: Int -> Text
tshow = T.pack . show
