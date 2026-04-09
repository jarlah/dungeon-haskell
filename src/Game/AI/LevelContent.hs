{-# LANGUAGE OverloadedStrings #-}
-- | LLM-driven level flavor text.
--
--   This module generates one-sentence atmospheric descriptions of
--   dungeon rooms: "A low-ceilinged vault where the torchlight gives
--   up before it reaches the far wall.", "Stale water pools between
--   cracked flagstones, and the rats have found something worth
--   gnawing.", and so on. The descriptions are purely cosmetic —
--   they don't affect gameplay — and are shown in the message log
--   when the player first steps into a room.
--
--   Shape mirrors 'Game.AI.QuestGen' and 'Game.AI.NPCBehaviour':
--   pure prompt construction, IO-bound send, pure post-processing.
--   Failures collapse to 'Nothing' so callers can silently fall
--   back (in this case, to showing no description at all — a
--   missing flavor line is less jarring than a wrong one).
module Game.AI.LevelContent
  ( describeRoom
  , cleanDescription
  ) where

import qualified Data.Text          as T
import           Data.Text          (Text)

import           Game.AI.Client     (AIClient, sendPrompt)
import           Game.AI.Prompts    (roomDescPrompt)
import           Game.Config        (AIConfig)
import           Game.Logic.Dungeon (Room (..))

-- | Ask the LLM for a one-sentence description of a room. The
--   monster name list is so the model can mention (or deliberately
--   foreshadow) what's already lurking there. Returns 'Nothing' on
--   any failure.
describeRoom
  :: AIClient
  -> AIConfig
  -> Room
  -> Int        -- ^ current dungeon depth
  -> [Text]     -- ^ names of monsters in the room
  -> IO (Maybe Text)
describeRoom client cfg room depth monsters = do
  let prompt = roomDescPrompt (rW room) (rH room) depth monsters
  result <- sendPrompt client cfg prompt
  pure $ case result of
    Left _    -> Nothing
    Right txt ->
      let cleaned = cleanDescription txt
      in if T.null cleaned then Nothing else Just cleaned

-- | Post-process a room description:
--
--   * trim whitespace
--   * collapse any internal newlines (the message log is
--     single-line per entry)
--   * drop a single wrapping pair of quotes if the model added them
--   * clamp the length so an overexcited reply can't flood the log
--
--   Split out so tests can pin the behavior on fixture strings.
cleanDescription :: Text -> Text
cleanDescription =
    clampLength 200
  . T.unwords
  . T.words
  . stripWrappingQuotes
  . T.strip
  where
    clampLength n t
      | T.length t <= n = t
      | otherwise       = T.take (n - 1) t <> "\x2026"

-- | Strip one matched pair of wrapping quote characters.
stripWrappingQuotes :: Text -> Text
stripWrappingQuotes t
  | T.length t >= 2
  , Just (h, mid) <- T.uncons t
  , Just (body, l) <- T.unsnoc mid
  , isQuote h
  , isQuote l
  , h == l
      = T.strip body
  | otherwise = t
  where
    isQuote c = c == '"' || c == '\'' || c == '\x201c' || c == '\x201d'
