{-# LANGUAGE ScopedTypeVariables #-}
-- | Simple file logger for the AI subsystem.
--
--   Writes timestamped lines to @ai.log@ in the current working
--   directory.  The file is opened in append mode and flushed after
--   every write so entries survive a crash.  Access is serialised
--   through an 'MVar' so concurrent callers (main thread + worker
--   thread) never interleave mid-line.
--
--   Deliberately minimal — no dependencies beyond @base@, @time@,
--   and @directory@.  The game can always fall back to no logging
--   if the file can't be opened (permissions, read-only FS, …).
module Game.AI.Log
  ( AILog
  , openAILog
  , closeAILog
  , logAI
  ) where

import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Exception        (SomeException, catch)
import Data.Time.Clock          (getCurrentTime)
import Data.Time.Format         (defaultTimeLocale, formatTime)
import System.IO
  ( Handle, IOMode (..), hClose, hFlush, hPutStrLn
  , openFile
  )

-- | Opaque handle to the log file.  'Nothing' means logging was
--   requested but the file couldn't be opened — every 'logAI' call
--   becomes a no-op.
newtype AILog = AILog (MVar (Maybe Handle))

-- | Open (or create) @ai.log@ in append mode.  Returns an 'AILog'
--   that is safe to use from any thread.  If the file can't be
--   opened the resulting 'AILog' silently swallows all writes.
openAILog :: IO AILog
openAILog = do
  mh <- (Just <$> openFile "ai.log" AppendMode)
           `catch` (\(_ :: SomeException) -> pure Nothing)
  AILog <$> newMVar mh

-- | Flush and close the log file.  Safe to call more than once.
closeAILog :: AILog -> IO ()
closeAILog (AILog mv) = do
  mh <- takeMVar mv
  case mh of
    Nothing -> putMVar mv Nothing
    Just h  -> do
      hFlush h `catch` (\(_ :: SomeException) -> pure ())
      hClose h `catch` (\(_ :: SomeException) -> pure ())
      putMVar mv Nothing

-- | Write a single timestamped line.  No-op if the log file
--   couldn't be opened or has been closed.
logAI :: AILog -> String -> IO ()
logAI (AILog mv) msg = do
  mh <- takeMVar mv
  case mh of
    Nothing -> putMVar mv Nothing
    Just h  -> do
      ts <- formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime
      (hPutStrLn h (ts ++ " " ++ msg) >> hFlush h)
        `catch` (\(_ :: SomeException) -> pure ())
      putMVar mv (Just h)
