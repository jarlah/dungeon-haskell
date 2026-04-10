{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Game configuration loaded from @config.yaml@.
--
--   The game ships with sensible defaults so running @dungeon-haskell@
--   without a config file Just Works. A config file (picked up in the
--   search order documented on 'loadConfig') overrides individual
--   fields; missing fields fall back to 'defaultGameConfig'.
--
--   All runtime knobs — AI endpoint, audio volumes, anything that
--   needs to differ between a dev laptop and a cold install — live
--   here, explicitly *not* in environment variables. A game is a
--   user-space application, not a server daemon, so a single
--   committed-to-disk config file is the right shape for this.
module Game.Config
  ( -- * Types
    GameConfig (..)
  , AIConfig (..)
  , AudioConfig (..)
  , AIProvider (..)
    -- * Defaults
  , defaultGameConfig
  , defaultAIConfig
  , defaultAudioConfig
    -- * Loading
  , loadConfig
  , loadConfigFrom
  , resolveConfigPath
  ) where

import           Control.Exception    (IOException, try)
import           Data.Aeson           (FromJSON (..), withObject, withText, (.!=), (.:?))
import qualified Data.Text            as T
import           Data.Text            (Text)
import qualified Data.Yaml            as Yaml
import           GHC.Generics         (Generic)
import           System.Directory     (XdgDirectory (..), doesFileExist, getXdgDirectory)
import           System.FilePath      ((</>))

-- | The full top-level config shape. Split into feature-group
--   sub-records so that adding a new subsystem does not require
--   touching existing fields and so that YAML keys group nicely.
data GameConfig = GameConfig
  { gcAI    :: !AIConfig
  , gcAudio :: !AudioConfig
  } deriving (Eq, Show, Generic)

-- | Which backend the AI client speaks to.
--
--   * 'ProviderMock' — use the in-process mock. No network calls,
--     deterministic canned responses. The default, so a first-time
--     player never needs to run an LLM. Also what the test suite
--     uses.
--   * 'ProviderOllama' — POST to a local Ollama @\/api\/generate@
--     endpoint.
--   * 'ProviderOpenAI' — POST to an OpenAI-compatible
--     @\/v1\/chat\/completions@ endpoint. Same shape covers proxies,
--     LM Studio, and other compatible backends.
--   * 'ProviderAnthropic' — POST to the Anthropic Messages API at
--     @\/v1\/messages@. Uses @x-api-key@ + @anthropic-version@ headers.
data AIProvider
  = ProviderMock
  | ProviderOllama
  | ProviderOpenAI
  | ProviderAnthropic
  deriving (Eq, Show)

-- | AI subsystem config. Mirrors the YAML layout documented in
--   @config.yaml.example@.
data AIConfig = AIConfig
  { aiEnabled        :: !Bool
    -- ^ master switch. 'False' forces the AI layer into a no-op
    --   path — no requests fire, no background threads, no
    --   canned fallback either. With @enabled: false@ the game is
    --   identical to pre-milestone-14 behavior.
  , aiProvider       :: !AIProvider
    -- ^ which backend the client speaks to
  , aiEndpoint       :: !Text
    -- ^ full URL to POST to. Ignored by the mock provider.
  , aiApiKey         :: !Text
    -- ^ bearer token. Empty string means "no auth header", which
    --   is what local Ollama wants.
  , aiModel          :: !Text
    -- ^ model identifier passed to the backend
  , aiTimeoutSeconds :: !Int
    -- ^ per-request timeout, seconds. Applied to the request
    --   middleware stack via 'withTimeout'.
  , aiMaxRetries     :: !Int
    -- ^ how many times to retry a failed request before giving up
    --   and returning the fallback content. Retries use exponential
    --   backoff starting at 500ms.
  } deriving (Eq, Show)

-- | Audio subsystem config. Parked here alongside 'AIConfig' so
--   that adding a volume slider later is a config-file edit rather
--   than an environment-variable scavenger hunt.
data AudioConfig = AudioConfig
  { acMusicVolume :: !Double
    -- ^ 0.0 .. 1.0 multiplier on music loop volume
  , acSfxVolume   :: !Double
    -- ^ 0.0 .. 1.0 multiplier on combat SFX volume
  } deriving (Eq, Show)

--------------------------------------------------------------------
-- Defaults
--------------------------------------------------------------------

-- | Complete default configuration. Used when no config file is
--   present and as the base onto which a partial config file is
--   merged (the FromJSON instance fills missing keys from this).
defaultGameConfig :: GameConfig
defaultGameConfig = GameConfig
  { gcAI    = defaultAIConfig
  , gcAudio = defaultAudioConfig
  }

-- | Safe out-of-the-box AI settings: mock provider, disabled. A
--   first-time launch never reaches out to the network.
defaultAIConfig :: AIConfig
defaultAIConfig = AIConfig
  { aiEnabled        = False
  , aiProvider       = ProviderMock
  , aiEndpoint       = "http://localhost:11434/api/generate"
  , aiApiKey         = ""
  , aiModel          = "mistral"
  , aiTimeoutSeconds = 15
  , aiMaxRetries     = 3
  }

defaultAudioConfig :: AudioConfig
defaultAudioConfig = AudioConfig
  { acMusicVolume = 0.7
  , acSfxVolume   = 1.0
  }

--------------------------------------------------------------------
-- JSON / YAML decoding
--------------------------------------------------------------------
-- Every FromJSON instance here is written by hand (no Generic
-- deriving) because we want *partial* configs to be valid: if the
-- user only sets `ai.enabled`, we fill in every other key from
-- 'defaultGameConfig' rather than erroring out.

instance FromJSON GameConfig where
  parseJSON = withObject "GameConfig" $ \o -> do
    ai    <- o .:? "ai"    .!= gcAI    defaultGameConfig
    audio <- o .:? "audio" .!= gcAudio defaultGameConfig
    pure GameConfig { gcAI = ai, gcAudio = audio }

instance FromJSON AIConfig where
  parseJSON = withObject "AIConfig" $ \o -> do
    enabled  <- o .:? "enabled"         .!= aiEnabled        defaultAIConfig
    provider <- o .:? "provider"        .!= aiProvider       defaultAIConfig
    endpoint <- o .:? "endpoint"        .!= aiEndpoint       defaultAIConfig
    apiKey   <- o .:? "api_key"         .!= aiApiKey         defaultAIConfig
    model    <- o .:? "model"           .!= aiModel          defaultAIConfig
    timeout  <- o .:? "timeout_seconds" .!= aiTimeoutSeconds defaultAIConfig
    retries  <- o .:? "max_retries"     .!= aiMaxRetries     defaultAIConfig
    pure AIConfig
      { aiEnabled        = enabled
      , aiProvider       = provider
      , aiEndpoint       = endpoint
      , aiApiKey         = apiKey
      , aiModel          = model
      , aiTimeoutSeconds = timeout
      , aiMaxRetries     = retries
      }

instance FromJSON AudioConfig where
  parseJSON = withObject "AudioConfig" $ \o -> do
    music <- o .:? "music_volume" .!= acMusicVolume defaultAudioConfig
    sfx   <- o .:? "sfx_volume"   .!= acSfxVolume   defaultAudioConfig
    pure AudioConfig { acMusicVolume = clamp01 music, acSfxVolume = clamp01 sfx }

-- | Clamp a volume value to the 0.0–1.0 range.
clamp01 :: Double -> Double
clamp01 = max 0.0 . min 1.0

instance FromJSON AIProvider where
  parseJSON = withText "AIProvider" $ \t -> case T.toLower t of
    "mock"   -> pure ProviderMock
    "ollama" -> pure ProviderOllama
    "openai"    -> pure ProviderOpenAI
    "anthropic" -> pure ProviderAnthropic
    other       -> fail $ "Unknown AI provider: " <> T.unpack other

--------------------------------------------------------------------
-- Loading
--------------------------------------------------------------------

-- | Resolve the config path by searching these locations, in order:
--
--   1. Path passed via @--config@ CLI flag (threaded in by the caller)
--   2. @./config.yaml@ relative to the current working directory
--   3. @$XDG_CONFIG_HOME/dungeon-haskell/config.yaml@
--
--   Returns the first path that exists, or 'Nothing' if none do —
--   in which case 'loadConfig' returns 'defaultGameConfig'.
resolveConfigPath :: Maybe FilePath -> IO (Maybe FilePath)
resolveConfigPath cliPath = case cliPath of
  Just p  -> do
    exists <- doesFileExist p
    pure $ if exists then Just p else Nothing
  Nothing -> do
    -- Try cwd first so a developer running from the project root
    -- can tweak a local config without touching ~/.config.
    let cwdPath = "config.yaml"
    cwdExists <- doesFileExist cwdPath
    if cwdExists
      then pure (Just cwdPath)
      else do
        xdgBase <- getXdgDirectory XdgConfig "dungeon-haskell"
        let xdgPath = xdgBase </> "config.yaml"
        xdgExists <- doesFileExist xdgPath
        pure $ if xdgExists then Just xdgPath else Nothing

-- | Top-level config loader. Resolves the config path, reads it if
--   it exists, falls back to defaults otherwise. Any parse error
--   is reported as 'Left' so the caller can surface it to the
--   player rather than silently running on defaults.
loadConfig :: Maybe FilePath -> IO (Either String GameConfig)
loadConfig cliPath = do
  mPath <- resolveConfigPath cliPath
  case mPath of
    Nothing -> pure (Right defaultGameConfig)
    Just p  -> loadConfigFrom p

-- | Load (and parse) a config file from a specific path. Unlike
--   'loadConfig' this does not do any search — callers use it when
--   they already know which file to read. Useful in tests.
loadConfigFrom :: FilePath -> IO (Either String GameConfig)
loadConfigFrom path = do
  r <- try (Yaml.decodeFileEither path) :: IO (Either IOException (Either Yaml.ParseException GameConfig))
  pure $ case r of
    Left ioErr         -> Left ("Config IO error: " <> show ioErr)
    Right (Left pErr)  -> Left ("Config parse error: " <> Yaml.prettyPrintParseException pErr)
    Right (Right cfg)  -> Right cfg
