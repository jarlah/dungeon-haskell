-- | IO shell for audio playback. Pure game logic emits 'GameEvent's;
--   this module is the only place that touches the audio device.
--
--   Everything here is graceful-degradation: if the audio device can't
--   be initialized, or a sample file is missing, the game keeps running
--   silently. A missing or broken audio backend must never crash the
--   game or block the render loop.
module Game.Audio
  ( AudioSystem
  , MusicTrack(..)
  , initAudio
  , shutdownAudio
  , playEvent
  , setMusic
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Exception (try, SomeException)
import Control.Monad (forM_, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Sound.ProteaAudio.SDL as PA

import Game.Config (AudioConfig(..))
import Game.Types (GameEvent(..))

-- | Which music loop is currently playing. The audio shell keeps a
--   reference to the currently looping 'PA.Sound' and cross-fades to
--   another when the game requests a track change via 'setMusic'.
data MusicTrack = DungeonMusic | BossMusic
  deriving (Eq, Show)

-- | Mutable audio playback state. The fade thread (if any) runs on a
--   background worker: it gradually ramps @msPrevSound@ down to 0
--   and @msCurrSound@ up to 1 over 'fadeDurationMs', then stops the
--   previous sound. A new 'setMusic' request interrupts the fade
--   (via 'killThread') and replaces the state; the half-faded
--   previous sound is hard-stopped to avoid stacking.
data MusicState = MusicState
  { msCurrTrack  :: !MusicTrack
  , msCurrSound  :: !PA.Sound
  , msPrevSound  :: !(Maybe PA.Sound)
    -- ^ the previously-playing loop while a fade is in progress
  , msFadeThread :: !(Maybe ThreadId)
  , msFadeGen    :: !Int
    -- ^ monotonically increasing generation counter for fade
    --   bookkeeping. Every 'setMusic' bumps it; the fade worker
    --   captures the generation at fork time and only clears its
    --   own state if the generation hasn't moved on under its
    --   feet (i.e., no newer fade has taken over).
  }

-- | Loaded sample handles plus the currently-looping music state.
--   'asMusicRef' is mutable so the game can swap tracks mid-run
--   (boss encounter) without tearing down the whole audio system.
data AudioSystem = AudioSystem
  { asMiss         :: !PA.Sample
  , asHit          :: !PA.Sample
  , asCrit         :: !PA.Sample
  , asKill         :: !PA.Sample
  , asHurt         :: !PA.Sample
  , asDied         :: !PA.Sample
  , asLevelUp      :: !PA.Sample
  , asDungeonMusic :: !PA.Sample
  , asBossMusic    :: !PA.Sample
    -- ^ may alias 'asDungeonMusic' if the boss track asset is
    --   missing (graceful fallback: the loop stays the same but
    --   the plumbing still exercises the swap code path)
  , asMusicRef     :: !(IORef MusicState)
  }

-- | Total duration of a cross-fade, in milliseconds. Long enough
--   that the transition is audible (the dungeon theme visibly
--   ducks out, the boss theme visibly ramps in) but short enough
--   that the player isn't left waiting for the sting. ~800ms lines
--   up roughly with a single musical beat at typical BGM tempo.
fadeDurationMs :: Int
fadeDurationMs = 800

-- | Number of volume-update ticks spread across a full fade. A
--   step every ~20ms is smooth enough for a linear volume ramp
--   without flooding the audio thread with update calls.
fadeSteps :: Int
fadeSteps = 40

-- | Initialise the audio backend, load every sample, and start the
--   background music loop. Returns 'Nothing' if *anything* goes wrong
--   — the caller should fall back to silent gameplay in that case.
--   The 'AudioConfig' supplies global music and SFX volume multipliers
--   applied on top of each asset's baked-in relative volume.
initAudio :: AudioConfig -> IO (Maybe AudioSystem)
initAudio cfg = do
  ok <- PA.initAudio 32 44100 1024
  if not ok
    then pure Nothing
    else do
      let musicVol = realToFrac (acMusicVolume cfg)
          sfxVol   = realToFrac (acSfxVolume cfg)
      result <- (try $ do
        -- Music first: dungeon theme is required, boss theme is
        -- optional and falls back to the dungeon sample if missing
        -- so the swap code still exercises cleanly.
        dungeonMusic <- PA.sampleFromFile "assets/music/theme.ogg" (0.35 * musicVol)
        bossMusic    <- loadOrFallback "assets/music/boss.ogg" (0.35 * musicVol) dungeonMusic
        musicSound   <- PA.soundLoop dungeonMusic 1.0 1.0 0.0 1.0
        musicRef     <- newIORef MusicState
          { msCurrTrack  = DungeonMusic
          , msCurrSound  = musicSound
          , msPrevSound  = Nothing
          , msFadeThread = Nothing
          , msFadeGen    = 0
          }
        -- SFX.
        miss    <- PA.sampleFromFile "assets/sfx/miss.ogg"    (0.7  * sfxVol)
        hit     <- PA.sampleFromFile "assets/sfx/hit.ogg"     (0.8  * sfxVol)
        crit    <- PA.sampleFromFile "assets/sfx/crit.ogg"    (1.0  * sfxVol)
        kill    <- PA.sampleFromFile "assets/sfx/kill.ogg"    (0.9  * sfxVol)
        hurt    <- PA.sampleFromFile "assets/sfx/hurt.ogg"    (0.85 * sfxVol)
        died    <- PA.sampleFromFile "assets/sfx/died.ogg"    (1.0  * sfxVol)
        levelUp <- PA.sampleFromFile "assets/sfx/levelup.ogg" (0.9  * sfxVol)
        pure AudioSystem
          { asMiss         = miss
          , asHit          = hit
          , asCrit         = crit
          , asKill         = kill
          , asHurt         = hurt
          , asDied         = died
          , asLevelUp      = levelUp
          , asDungeonMusic = dungeonMusic
          , asBossMusic    = bossMusic
          , asMusicRef     = musicRef
          }) :: IO (Either SomeException AudioSystem)
      case result of
        Left _    -> PA.finishAudio >> pure Nothing
        Right sys -> pure (Just sys)

-- | Load a sample, falling back to a pre-loaded alternative if the
--   file can't be loaded. Used for the boss music so missing assets
--   degrade to the dungeon loop instead of killing audio init.
loadOrFallback :: FilePath -> Float -> PA.Sample -> IO PA.Sample
loadOrFallback path vol fallback = do
  r <- try (PA.sampleFromFile path vol) :: IO (Either SomeException PA.Sample)
  case r of
    Right s -> pure s
    Left _  -> pure fallback

-- | Tear down the audio system. Kills any in-flight fade thread
--   before tearing down the backend so the worker can't touch
--   freed sound handles after 'PA.finishAudio'.
shutdownAudio :: AudioSystem -> IO ()
shutdownAudio as = do
  ms <- readIORef (asMusicRef as)
  case msFadeThread ms of
    Just tid -> do
      _ <- (try (killThread tid)) :: IO (Either SomeException ())
      pure ()
    Nothing  -> pure ()
  PA.finishAudio

-- | Switch the background music loop to the requested track. No-op
--   if the requested track is already the /target/. On a real
--   change:
--
--   * any in-flight fade is interrupted (thread killed, the sound
--     it was fading out is hard-stopped);
--   * the new sample starts looping silently (volume 0);
--   * a fresh worker thread ramps the previous loop down and the
--     new loop up over 'fadeDurationMs'.
--
--   Exceptions from the audio layer are swallowed so a missing
--   asset or device hiccup can't crash the game mid-transition.
setMusic :: AudioSystem -> MusicTrack -> IO ()
setMusic as target = do
  ms <- readIORef (asMusicRef as)
  when (msCurrTrack ms /= target) $ do
    -- Interrupt any fade already in progress: kill the worker and
    -- hard-stop whatever it was fading out, so we don't leave an
    -- orphaned half-volume loop playing underneath the new fade.
    case msFadeThread ms of
      Just tid -> (try (killThread tid)
                   :: IO (Either SomeException ())) >> pure ()
      Nothing  -> pure ()
    case msPrevSound ms of
      Just s  -> (try (PA.soundStop s)
                   :: IO (Either SomeException Bool)) >> pure ()
      Nothing -> pure ()
    -- Start the new track silently so the ramp can bring it up.
    let sample = case target of
          DungeonMusic -> asDungeonMusic as
          BossMusic    -> asBossMusic    as
    r <- (try (PA.soundLoop sample 0.0 0.0 0.0 1.0))
           :: IO (Either SomeException PA.Sound)
    case r of
      Left _         -> pure ()
      Right newSound -> do
        let prev   = msCurrSound ms
            newGen = msFadeGen ms + 1
        tid <- forkIO (runFade (asMusicRef as) newGen prev newSound)
        atomicModifyIORef' (asMusicRef as) $ \_ ->
          ( MusicState
              { msCurrTrack  = target
              , msCurrSound  = newSound
              , msPrevSound  = Just prev
              , msFadeThread = Just tid
              , msFadeGen    = newGen
              }
          , ()
          )

-- | Linear cross-fade worker: ramps @prev@ from full volume down
--   to silence and @new@ from silence up to full volume in
--   'fadeSteps' equal ticks spanning 'fadeDurationMs'. At the end
--   it hard-stops @prev@ and clears the fade bookkeeping on the
--   shared state — but only if its own fade generation is still
--   current. Exceptions are swallowed (if the main thread has
--   already torn down audio, the worker just exits).
runFade :: IORef MusicState -> Int -> PA.Sound -> PA.Sound -> IO ()
runFade ref myGen prev new = do
  let stepUs = (fadeDurationMs * 1000) `div` fadeSteps
  _ <- (try $ forM_ [1 .. fadeSteps] $ \i -> do
          let t = fromIntegral i / fromIntegral fadeSteps :: Float
          _ <- PA.soundUpdate prev False (1 - t) (1 - t) 0 1
          _ <- PA.soundUpdate new  False t       t       0 1
          threadDelay stepUs) :: IO (Either SomeException ())
  _ <- (try (PA.soundStop prev)) :: IO (Either SomeException Bool)
  -- Only clear the fade bookkeeping if *this* fade generation is
  -- still the latest — otherwise a newer setMusic already swapped
  -- in a fresh thread + prevSound pair that we must not clobber.
  atomicModifyIORef' ref $ \ms ->
    if msFadeGen ms == myGen
      then (ms { msPrevSound = Nothing, msFadeThread = Nothing }, ())
      else (ms, ())

-- | Fire the SFX for a single 'GameEvent'. Non-blocking. Exceptions
--   from the audio layer are swallowed so a glitch can't crash the
--   game.
playEvent :: AudioSystem -> GameEvent -> IO ()
playEvent as ev = do
  let sample = case ev of
        EvAttackMiss    -> asMiss    as
        EvAttackHit     -> asHit     as
        EvAttackCrit    -> asCrit    as
        EvMonsterKilled -> asKill    as
        EvPlayerHurt    -> asHurt    as
        EvPlayerDied    -> asDied    as
        EvLevelUp       -> asLevelUp as
        -- Quest turn-in reuses the level-up jingle for now — it's a
        -- positive progression sting, which is the same emotional
        -- beat. A dedicated asset can slot in later without touching
        -- the event pump.
        EvQuestTurnedIn -> asLevelUp as
        -- The boss-kill sting piggybacks on the level-up jingle for
        -- now — it's the same "you won something big" beat. A
        -- dedicated victory fanfare can slot in later via a new
        -- AudioSystem field without touching the dispatch table.
        EvBossKilled    -> asLevelUp as
  _ <- (try (PA.soundPlay sample 1.0 1.0 0.0 1.0))
         :: IO (Either SomeException PA.Sound)
  pure ()
