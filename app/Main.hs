module Main (main) where

import Brick
import qualified Brick.BChan               as BChan
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Random (newStdGen)

import Game.AI.Runtime
  ( AIRuntime, AppEvent (..)
  , applyAIResponse, maybeFireGreeting, maybeFireQuest, maybeFireRoomDesc
  , startAIRuntime, stopAIRuntime
  )
import qualified Game.Audio as Audio
import qualified Game.Config as Config
import Game.GameState
import Game.Logic.Dungeon (defaultLevelConfig)
import Game.Render
  ( drawGame, bossAttr, doorAttr, lockedDoorAttr, fogAttr, npcAttr
  , saveMenuCursorAttr, saveMenuEmptyAttr
  , launchCursorAttr, launchDisabledAttr, launchTitleAttr
  )
import Game.UI.Launch (handleLaunchMenuKey)
import Game.UI.Modals
  ( handleAwaitingDirectionKey, handleConfirmQuitKey, handleDialogueKey
  , handleHelpKey, handleInventoryKey, handleLockedDoorKey
  , handleQuestLogKey, handleVictoryKey
  )
import Game.UI.Normal (handleNormalKey)
import Game.UI.Prompt (handlePromptKey)
import Game.UI.SaveMenu (handleSaveMenuKey, sampleHasSaves)
import Game.UI.Types (Name (..), RuntimeFlags (..), parseArgs)

-- | Build the Brick 'App' with the audio shell and AI runtime
--   closed into the event handler. Passing 'Nothing' for the audio
--   system disables audio playback (silent run). The 'AIRuntime' is
--   always supplied — its own config decides whether requests
--   actually fire.
mkApp
  :: Maybe Audio.AudioSystem
  -> AIRuntime
  -> RuntimeFlags
  -> App GameState AppEvent Name
mkApp mAudio aiRt rFlags = App
  { appDraw         = drawGame (rfWizardEnabled rFlags)
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleEvent mAudio aiRt rFlags
  , appStartEvent   = pure ()
  , appAttrMap      = const $ attrMap V.defAttr
      [ (fogAttr,             fg V.brightBlack)
      , (npcAttr,             fg V.yellow)
      , (bossAttr,            fg V.red)
      , (doorAttr,            fg V.brightYellow)
      , (lockedDoorAttr,      fg V.red)
      , (saveMenuCursorAttr,  V.defAttr `V.withStyle` V.reverseVideo)
      , (saveMenuEmptyAttr,   fg V.brightBlack)
      , (launchCursorAttr,    V.defAttr `V.withStyle` V.reverseVideo)
      , (launchDisabledAttr,  fg V.brightBlack)
      , (launchTitleAttr,     fg V.brightYellow `V.withStyle` V.bold)
      ]
  }

handleEvent
  :: Maybe Audio.AudioSystem
  -> AIRuntime
  -> RuntimeFlags
  -> BrickEvent Name AppEvent
  -> EventM Name GameState ()
handleEvent mAudio aiRt rFlags (VtyEvent (V.EvKey key mods)) = do
  gs <- get
  case () of
    -- Launch screen swallows everything — the player is not yet
    -- "in the game", so no other modal or gameplay key should fire
    -- until they pick an entry point.
    _ | Just lm  <- gsLaunchMenu gs -> handleLaunchMenuKey rFlags lm key
      | gsConfirmQuit gs            -> handleConfirmQuitKey key
      | gsVictory gs                -> handleVictoryKey key
      | gsHelpOpen gs               -> handleHelpKey key
      | Just sm  <- gsSaveMenu gs   -> handleSaveMenuKey rFlags sm key
      | Just buf <- gsPrompt gs     -> handlePromptKey mAudio rFlags key buf
      | Just i   <- gsDialogue gs   -> handleDialogueKey mAudio aiRt i key
      | gsQuestLogOpen gs           -> handleQuestLogKey key
      | gsInventoryOpen gs          -> handleInventoryKey mAudio key
      | Just _   <- gsLockedDoorPrompt gs ->
          handleLockedDoorKey key
      | Just dirAct <- gsAwaitingDirection gs ->
          handleAwaitingDirectionKey mAudio dirAct key
      | otherwise                   -> handleNormalKey mAudio aiRt rFlags key mods
  -- Every key event is an opportunity to swap music tracks — the
  -- player may have just walked into the boss room's line of sight,
  -- or descended onto the boss floor, or climbed back off it.
  updateMusicFor mAudio
  -- If that keystroke opened a dialogue with an NPC we haven't
  -- generated a greeting for yet, fire the request now. Safe to
  -- call unconditionally — the helper is a no-op when AI is
  -- disabled or the greeting is already cached.
  maybeFireGreeting aiRt
  -- Same deal for per-floor quest generation: every time the
  -- player is on a floor we haven't asked the LLM about yet,
  -- fire one 'ReqQuest'. The helper self-dedupes via
  -- 'aiSeenFloors' so revisiting doesn't spam the worker.
  maybeFireQuest aiRt
  -- And once more for room descriptions: whenever the player is
  -- standing in a room on the current floor that we haven't
  -- described yet, fire a 'ReqRoomDesc'. Dedup via 'aiSeenRooms'.
  maybeFireRoomDesc aiRt
handleEvent _ aiRt _ (AppEvent (AIResult resp)) =
  applyAIResponse aiRt resp
handleEvent _ _ _ _ = pure ()

-- | Consult the current 'GameState' and tell the audio shell which
--   music loop should be playing. The decision is delegated to
--   'shouldPlayBossMusic', which keeps Main.hs free of dungeon
--   geometry and set imports.
updateMusicFor :: Maybe Audio.AudioSystem -> EventM Name GameState ()
updateMusicFor Nothing      = pure ()
updateMusicFor (Just audio) = do
  gs <- get
  let track = if shouldPlayBossMusic gs
                then Audio.BossMusic
                else Audio.DungeonMusic
  liftIO $ Audio.setMusic audio track

--------------------------------------------------------------------
-- main
--------------------------------------------------------------------

main :: IO ()
main = do
  -- CLI flags first — a single boolean ride-along that decides
  -- whether this process is allowed to run wizard cheats and
  -- whether it's allowed to see cheat-tainted saves in the load
  -- menu. Kept deliberately hand-rolled (no optparse-applicative
  -- dependency) because we only need one flag and the parse is
  -- two lines.
  args <- getArgs
  let rFlags = parseArgs args

  -- Config next: a bad config file should fail loudly and early
  -- rather than partially running the game with surprise defaults.
  cfgRes <- Config.loadConfig Nothing
  gameCfg <- case cfgRes of
    Right c  -> pure c
    Left err -> do
      hPutStrLn stderr ("Warning: " <> err)
      hPutStrLn stderr "Continuing with default configuration."
      pure Config.defaultGameConfig

  gen <- newStdGen
  hasSaves <- sampleHasSaves rFlags
  -- The initial state is a fresh run with the launch screen
  -- layered on top. Picking /New Game/ drops the launch field and
  -- reveals this same state; picking /Continue/ or /Load/ replaces
  -- it wholesale with a deserialized 'GameState'. Sampling the
  -- save directory once at startup is enough because the menu
  -- stays open until the player commits to one path.
  let initialState = (newGame gen defaultLevelConfig)
        { gsLaunchMenu = Just LaunchMenu
            { lmCursor   = 0
            , lmHasSaves = hasSaves
            }
        }
      buildVty = VCP.mkVty V.defaultConfig

  -- A Brick 'BChan' bridges the AI worker thread and the main event
  -- loop: the worker emits 'AIResult' events, the event loop pulls
  -- them off alongside VtyEvents via 'customMain'.
  aiChan <- BChan.newBChan 16

  -- Audio init is best-effort: if it fails (no device, missing
  -- assets, ...), 'bracket' still runs the game silently.
  bracket Audio.initAudio
          (mapM_ Audio.shutdownAudio)
          $ \mAudio ->
    bracket (startAIRuntime gameCfg aiChan)
            stopAIRuntime
            $ \aiRt -> do
      initialVty <- buildVty
      _ <- customMain initialVty buildVty (Just aiChan)
             (mkApp mAudio aiRt rFlags) initialState
      pure ()
