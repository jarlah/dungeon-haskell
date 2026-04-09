{-# LANGUAGE OverloadedStrings #-}
-- | LLM-driven NPC flavor generation.
--
--   Right now this module has a single responsibility: produce a
--   one-to-two sentence in-character greeting for an NPC the player
--   is about to talk to. The hardcoded 'Game.GameState.npcGreeting'
--   field is the fallback; a successful LLM call overrides it for
--   the duration of the dialogue (and, once the async path lands,
--   gets cached on the NPC itself so re-opening the dialogue doesn't
--   hit the network twice).
--
--   The design mirrors 'Game.AI.QuestGen':
--
--   * pure prompt construction in "Game.AI.Prompts"
--   * IO-bound network send in "Game.AI.Client"
--   * pure post-processing here (whitespace, stray quotes, ...)
--
--   Failure collapses to 'Nothing' so callers can fall back
--   transparently without ever surfacing an error to the player.
module Game.AI.NPCBehaviour
  ( generateGreeting
  , cleanGreeting
  ) where

import qualified Data.Text          as T
import           Data.Text          (Text)

import           Game.AI.Client     (AIClient, sendPrompt)
import           Game.AI.Prompts    (greetingPrompt)
import           Game.Config        (AIConfig)

-- | Ask the LLM for a greeting for the given NPC. Returns 'Nothing'
--   on any failure — the caller falls back to the hardcoded
--   'Game.GameState.npcGreeting'.
--
--   The NPC role is free-form text ("Quest Master", "Merchant",
--   "Hermit", ...) — whatever the call site wants the model to
--   flavor the reply around. Depth is passed through so the model
--   can thread it into the atmosphere if it wants.
generateGreeting
  :: AIClient
  -> AIConfig
  -> Text       -- ^ NPC display name
  -> Text       -- ^ NPC role / descriptor
  -> Int        -- ^ current dungeon depth
  -> IO (Maybe Text)
generateGreeting client cfg name role depth = do
  let prompt = greetingPrompt name role depth
  result <- sendPrompt client cfg prompt
  pure $ case result of
    Left _    -> Nothing
    Right txt ->
      let cleaned = cleanGreeting txt
      in if T.null cleaned then Nothing else Just cleaned

-- | Post-process a greeting the LLM returned:
--
--   * strip surrounding whitespace
--   * drop matching leading/trailing quote characters the model
--     sometimes wraps its reply in despite the prompt saying not to
--   * collapse internal newlines into spaces so the dialogue header
--     stays one-lineable
--   * clamp to a sensible length so a runaway reply can't blow out
--     the dialogue box
--
--   Pulled out of 'generateGreeting' so tests can exercise it
--   against fixture strings directly.
cleanGreeting :: Text -> Text
cleanGreeting =
    clampLength 240
  . T.unwords
  . T.words
  . stripWrappingQuotes
  . T.strip
  where
    clampLength n t
      | T.length t <= n = t
      | otherwise       = T.take (n - 1) t <> "\x2026"  -- ellipsis

-- | Remove a single matched pair of wrapping quote characters from
--   the start and end of the string. Idempotent: if the string
--   isn't quoted, returns it unchanged.
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
