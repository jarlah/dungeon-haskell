{-# LANGUAGE OverloadedStrings #-}
-- | Non-blocking AI request pipeline.
--
--   The game's main thread is driven by a Brick event loop that
--   /must not block/ on network I/O — a slow LLM reply would
--   otherwise freeze the renderer and make the game feel broken.
--   This module runs the AI client on a dedicated worker thread
--   so the event loop stays responsive.
--
--   Architecture:
--
--   @
--       game thread                     AI worker thread
--       ───────────                     ────────────────
--       requestAI ──────▶ TQueue ────▶ readTQueue
--                                        │
--                                        │ sendPrompt (IO, slow)
--                                        ▼
--                                      emit (AIResponse -> IO ())
--   @
--
--   The @emit@ callback is injected by 'startAIWorker' at startup
--   so this module never needs to know about the game's top-level
--   event type. In practice Main.hs gives it a callback that wraps
--   the response in the Brick custom-event variant and writes it
--   to the application's @BChan@.
--
--   Design notes:
--
--   * A single worker thread is enough for a single-player game.
--     Multiple in-flight requests queue up; the player is unlikely
--     to trigger more than one or two a minute.
--   * The queue is unbounded — an 'STM.TQueue' — because we never
--     produce requests faster than the player can click, so
--     backpressure isn't meaningful.
--   * 'stopAIWorker' is idempotent and synchronous: it cancels the
--     worker and waits for it to exit so the caller can be sure no
--     more responses will be emitted afterwards.
module Game.AI.Async
  ( AIWorker
  , startAIWorker
  , stopAIWorker
  , requestAI
  ) where

import           Control.Concurrent.Async (Async, async, cancel, waitCatch)
import           Control.Concurrent.STM   (TQueue, atomically, newTQueueIO,
                                           readTQueue, writeTQueue)
import           Control.Exception        (SomeException, try)
import           Control.Monad            (forever)
import qualified Data.Text                as T
import           Data.Text                (Text)

import           Game.AI.Client           (AIClient, sendPrompt)
import           Game.AI.Types            (AIError (..), AIRequest (..),
                                           AIResponse (..))
import           Game.Config              (AIConfig)

-- | Handle to a running AI worker. Opaque to callers — the only
--   things they can do with it are submit a request ('requestAI')
--   and stop it ('stopAIWorker').
data AIWorker = AIWorker
  { workerQueue  :: !(TQueue AIRequest)
  , workerThread :: !(Async ())
  }

-- | Spawn the AI worker thread. Returns immediately. The worker
--   will call the supplied @emit@ callback once for each request
--   it processes, in the order requests arrive.
--
--   The callback is called from the worker thread — keep it cheap
--   and non-blocking. In practice it should just push the response
--   onto the Brick 'BChan' and return.
startAIWorker
  :: AIClient
  -> AIConfig
  -> (AIResponse -> IO ())   -- ^ emit a response back to the game layer
  -> IO AIWorker
startAIWorker client cfg emit = do
  q <- newTQueueIO
  tid <- async (workerLoop client cfg q emit)
  pure AIWorker { workerQueue = q, workerThread = tid }

-- | Stop the worker and wait for it to exit. Safe to call more
--   than once (subsequent calls are no-ops because 'cancel' of an
--   already-finished 'Async' is a no-op and 'waitCatch' returns
--   immediately).
stopAIWorker :: AIWorker -> IO ()
stopAIWorker w = do
  cancel (workerThread w)
  _ <- waitCatch (workerThread w)
  pure ()

-- | Enqueue a request for the worker to process. Returns
--   immediately — the game thread never blocks on the network.
requestAI :: AIWorker -> AIRequest -> IO ()
requestAI w req = atomically (writeTQueue (workerQueue w) req)

--------------------------------------------------------------------
-- Worker loop
--------------------------------------------------------------------

-- | The worker thread's main loop. Reads one request at a time,
--   dispatches it through the AI client, wraps the result in the
--   matching 'AIResponse' variant, and hands it off to the emit
--   callback.
--
--   Any exception thrown while processing a single request is
--   caught and converted into an 'AIError' so the game layer
--   still gets /a/ response for every request it submitted —
--   otherwise a crash in the middleware stack could leave the
--   UI waiting forever.
workerLoop
  :: AIClient
  -> AIConfig
  -> TQueue AIRequest
  -> (AIResponse -> IO ())
  -> IO ()
workerLoop client cfg q emit = forever $ do
  req <- atomically (readTQueue q)
  resp <- processRequest client cfg req
  emit resp

-- | Run a single request through the client and wrap the result.
--   Catches synchronous exceptions so the worker loop can't die
--   on a badly-configured endpoint or a surprise runtime error.
processRequest :: AIClient -> AIConfig -> AIRequest -> IO AIResponse
processRequest client cfg req = do
  let (wrap, prompt) = case req of
        ReqGreeting  tok p -> (RespGreeting  tok, p)
        ReqQuest     tok p -> (RespQuest     tok, p)
        ReqRoomDesc  tok p -> (RespRoomDesc  tok, p)
  result <- safeSend client cfg prompt
  pure (wrap result)

-- | 'sendPrompt' with a catch-all wrapper. Synchronous exceptions
--   become 'AINetworkError' so they flow through the same handling
--   path as every other failure reason.
safeSend :: AIClient -> AIConfig -> Text -> IO (Either AIError Text)
safeSend client cfg prompt = do
  r <- try (sendPrompt client cfg prompt) :: IO (Either SomeException (Either AIError Text))
  pure $ case r of
    Left ex       -> Left (AINetworkError (T.pack (show ex)))
    Right inner   -> inner
