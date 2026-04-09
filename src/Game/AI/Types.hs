{-# LANGUAGE OverloadedStrings #-}
-- | Shared AI subsystem types.
--
--   Lives in its own module to break what would otherwise be an
--   import cycle between "Game.AI.Client" (which produces these) and
--   "Game.AI.Async" / "Game.AI.Mock" (which also produce them).
module Game.AI.Types
  ( AIError (..)
  , displayAIError
  , AIRequest (..)
  , AIResponse (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Everything that can go wrong on the AI path. The game layer
--   never needs to unpack the specific middleware error — it only
--   cares enough to log the cause and fall back to hardcoded content.
data AIError
  = AINetworkError !Text
    -- ^ transport-level failure: connection refused, DNS miss, ...
  | AITimeout
    -- ^ request exceeded the per-call timeout
  | AIRetryExhausted !Text
    -- ^ retry middleware gave up; the string is the last underlying
    --   error's display form
  | AICircuitOpen
    -- ^ circuit breaker has tripped and is refusing calls
  | AIHttpStatus !Int !Text
    -- ^ backend returned a non-2xx HTTP status
  | AIParseError !Text
    -- ^ response body could not be parsed into the expected shape
    --   (JSON decode failure, schema mismatch, ...)
  | AIDisabled
    -- ^ AI is switched off in config; callers should fall back to
    --   hardcoded content without logging this as an error
  deriving (Eq, Show)

-- | Render an 'AIError' as a single short line suitable for the
--   message log. Kept close to the type so every call site uses the
--   same phrasing.
displayAIError :: AIError -> Text
displayAIError err = case err of
  AINetworkError t      -> "network error: " <> t
  AITimeout             -> "AI request timed out"
  AIRetryExhausted t    -> "AI retries exhausted: " <> t
  AICircuitOpen         -> "AI backend circuit breaker open"
  AIHttpStatus c t      -> "AI HTTP " <> T.pack (show c) <> ": " <> t
  AIParseError t        -> "AI parse error: " <> t
  AIDisabled            -> "AI disabled"

-- | A request fired from the game layer into the async AI pipeline.
--   Each variant carries a correlation token (an 'Int' — monotonic
--   per-request, assigned by the caller) so the response handler can
--   match results back to the originating site without the game
--   layer having to remember an 'MVar' or callback.
data AIRequest
  = ReqGreeting
      !Int      -- ^ correlation token (typically the NPC index)
      !Text     -- ^ prompt text
  | ReqQuest
      !Int      -- ^ correlation token (typically the NPC index)
      !Text     -- ^ prompt text
  | ReqRoomDesc
      !Int      -- ^ correlation token (typically the room index)
      !Text     -- ^ prompt text
  deriving (Eq, Show)

-- | Response from an 'AIRequest'. Each variant carries the same
--   correlation token the request had, plus either a successful
--   payload or an 'AIError'. The game layer handles success by
--   folding the content into the right 'GameState' field and
--   failure by logging and falling back.
data AIResponse
  = RespGreeting
      !Int
      !(Either AIError Text)
  | RespQuest
      !Int
      !(Either AIError Text)    -- ^ raw JSON text; parsing happens in QuestGen
  | RespRoomDesc
      !Int
      !(Either AIError Text)
  deriving (Eq, Show)
