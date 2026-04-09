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
import Game.Input (handleKey)
import Game.Logic.Command (Command(..), parseCommand, isCheatCommand)
import Game.Logic.Dungeon (defaultLevelConfig)
import Game.Render
  ( drawGame, bossAttr, doorAttr, fogAttr, npcAttr
  , saveMenuCursorAttr, saveMenuEmptyAttr
  , launchCursorAttr, launchDisabledAttr, launchTitleAttr
  )
import qualified Game.Save as Save
import Game.UI.Launch (handleLaunchMenuKey)
import Game.UI.Modals
  ( handleAwaitingDirectionKey, handleConfirmQuitKey, handleDialogueKey
  , handleHelpKey, handleInventoryKey, handleQuestLogKey, handleVictoryKey
  , playEventsFor
  )
import Game.UI.SaveMenu (handleSaveMenuKey, openSaveMenu, sampleHasSaves)
import Game.UI.Types (Name (..), RuntimeFlags (..), parseArgs)
import Game.Types (GameAction(..))

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

-- | Keystrokes while the slash-command prompt is open. The prompt
--   swallows all input: 'Esc' cancels, 'Enter' submits and
--   dispatches, 'Backspace' edits, printable characters append.
--   Nothing else advances the game.
--
--   Dispatch splits two ways. Safe UI commands (@/help@, @/save@,
--   @/wait@, ...) are handled inline because several of them need
--   to open modals or touch the filesystem. Wizard / cheat
--   commands go through 'applyCommand' and are refused unless the
--   game was launched with @--wizard@ (surfaced in 'rFlags').
handlePromptKey
  :: Maybe Audio.AudioSystem
  -> RuntimeFlags
  -> V.Key
  -> String
  -> EventM Name GameState ()
handlePromptKey mAudio rFlags key buf = case key of
  V.KEsc ->
    modify (\gs -> gs { gsPrompt = Nothing })
  V.KEnter -> do
    modify (\gs -> gs { gsPrompt = Nothing })
    case parseCommand buf of
      Right cmd -> dispatchCommand mAudio rFlags cmd
      Left err ->
        modify (\gs -> gs { gsMessages = ("Error: " ++ err) : gsMessages gs })
  V.KBS ->
    modify (\gs -> gs { gsPrompt = Just (dropLast buf) })
  V.KChar c ->
    modify (\gs -> gs { gsPrompt = Just (buf ++ [c]) })
  _ ->
    pure ()
  where
    dropLast [] = []
    dropLast xs = init xs

-- | Route a parsed 'Command' to the right handler. Safe UI
--   commands open modals or call the filesystem helpers inline;
--   wizard cheats flow through 'applyCommand' after a capability
--   check against 'rfWizardEnabled'.
dispatchCommand
  :: Maybe Audio.AudioSystem
  -> RuntimeFlags
  -> Command
  -> EventM Name GameState ()
dispatchCommand mAudio rFlags cmd = case cmd of
  -- Safe UI shortcuts: these are alternate paths to things the
  -- player could already trigger with a keybind. No cheat check.
  CmdHelp ->
    modify (\gs -> gs { gsHelpOpen = True })
  CmdQuit ->
    modify (\gs -> gs { gsConfirmQuit = True })
  CmdInventory ->
    modify (\gs -> gs { gsInventoryOpen = True })
  CmdQuests ->
    modify $ \gs -> gs
      { gsQuestLogOpen   = True
      , gsQuestLogCursor = Nothing
      }
  CmdWait -> do
    modify (applyAction Wait)
    playEventsFor mAudio
  CmdSave       -> openSaveMenu rFlags SaveMode
  CmdLoad       -> openSaveMenu rFlags LoadMode
  CmdQuicksave  -> doQuicksave
  CmdQuickload  -> doQuickload
  -- Wizard cheats: gated on the runtime capability flag.
  _
    | isCheatCommand cmd, not (rfWizardEnabled rFlags) ->
        modify $ \gs -> gs
          { gsMessages =
              "Cheats are disabled. Launch with --wizard to enable."
                : gsMessages gs
          }
    | otherwise ->
        modify (applyCommand cmd)

-- | Keystrokes while the prompt is closed. @/@ opens the prompt;
--   @i@ opens the inventory modal; everything else goes through the
--   normal action keymap.
handleNormalKey
  :: Maybe Audio.AudioSystem
  -> AIRuntime
  -> RuntimeFlags
  -> V.Key
  -> [V.Modifier]
  -> EventM Name GameState ()
-- Esc at the top priority: if the AI room-description panel is
-- currently drawn, dismiss it without confirming quit. This keeps
-- Esc's usual modal-dismiss semantics working for the flavor
-- panel. Only if the panel is not visible does Esc fall through
-- to the generic 'Quit' mapping in 'handleKey' below.
handleNormalKey _ _ _ V.KEsc _ = do
  gs <- get
  if gsRoomDescVisible gs
    then modify (\s -> s { gsRoomDescVisible = False })
    else modify (\s -> s { gsConfirmQuit = True })
handleNormalKey _ _ _ (V.KChar '/') _ =
  modify (\gs -> gs { gsPrompt = Just "" })
handleNormalKey _ _ _ (V.KChar '?') _ =
  modify (\gs -> gs { gsHelpOpen = True })
handleNormalKey _ _ _ (V.KChar 'i') _ =
  modify (\gs -> gs { gsInventoryOpen = True })
-- 'c' enters the two-step close-door mode: prompt for a direction
-- key, then dispatch 'CloseDoor' on the next keystroke. No turn is
-- consumed here — the turn is spent (or not) when the direction
-- actually resolves in 'handleAwaitingDirectionKey'.
handleNormalKey _ _ _ (V.KChar 'c') _ =
  modify $ \gs -> gs
    { gsAwaitingDirection = Just DirCloseDoor
    , gsMessages = "Close door in which direction?" : gsMessages gs
    }
handleNormalKey _ _ _ (V.KChar 'Q') _ =
  modify (\gs -> gs { gsQuestLogOpen = True, gsQuestLogCursor = Nothing })
-- Quicksave (F5) and quickload (F9) are free actions: they do not
-- advance monsters and do not run through 'applyAction' — they
-- talk directly to the filesystem and report into 'gsMessages'.
handleNormalKey _ _ _ (V.KFun 5) _ = doQuicksave
handleNormalKey _ _ _ (V.KFun 9) _ = doQuickload
-- F2 / F3 open the full save and load picker modals respectively.
-- Both take a snapshot of the save directory at open time so the
-- entry list doesn't shift under the cursor mid-menu.
handleNormalKey _ _ rFlags (V.KFun 2) _ = openSaveMenu rFlags SaveMode
handleNormalKey _ _ rFlags (V.KFun 3) _ = openSaveMenu rFlags LoadMode
handleNormalKey mAudio _ _ key mods =
  case handleKey key mods of
    Just Quit ->
      -- Don't halt immediately — open a confirm modal. q and Q
      -- are one shift-key apart, so fat-fingering Quest Log
      -- would otherwise kill the run.
      modify (\gs -> gs { gsConfirmQuit = True })
    Just act  -> do
      modify (applyAction act)
      playEventsFor mAudio
    Nothing   -> pure ()

-- | Save the current 'GameState' to the quicksave slot and report
--   the outcome into the message log. Quicksave is a free action —
--   it does not emit game events, does not clear the event queue,
--   and does not advance monsters. On failure the game continues
--   untouched with an error line in the log so the player can see
--   what went wrong.
doQuicksave :: EventM Name GameState ()
doQuicksave = do
  gs  <- get
  res <- liftIO (Save.writeSave Save.QuickSlot gs)
  let line = case res of
        Right ()                   -> "Quicksaved."
        Left Save.SaveMissing      -> "Quicksave failed: save directory missing."
        Left Save.SaveWrongMagic   -> "Quicksave failed: internal error (magic)."
        Left Save.SaveWrongVersion -> "Quicksave failed: internal error (version)."
        Left (Save.SaveCorrupt e)  -> "Quicksave failed: " ++ e
        Left (Save.SaveIOError e)  -> "Quicksave failed: " ++ e
  modify (\s -> s { gsMessages = line : gsMessages s })

-- | Replace the current 'GameState' with whatever is in the
--   quicksave slot. No monster advancement, no event emission —
--   the loaded state /is/ the new snapshot, modal flags included
--   (so quicksaving with the inventory open and quickloading
--   re-opens it). On failure the current state is preserved and
--   an error line is pushed into the message log.
doQuickload :: EventM Name GameState ()
doQuickload = do
  res <- liftIO (Save.readSave Save.QuickSlot)
  case res of
    Right loaded ->
      -- The loaded state ships with its own message list; we prepend
      -- a breadcrumb so the player can see load happened without
      -- losing the saved history.
      put loaded { gsMessages = "Quickloaded." : gsMessages loaded }
    Left err -> do
      let line = case err of
            Save.SaveMissing       -> "No quicksave to load."
            Save.SaveWrongMagic    -> "Quicksave is not a valid save file."
            Save.SaveWrongVersion  -> "Quicksave is from an older version of the game."
            Save.SaveCorrupt e     -> "Quicksave is corrupted: " ++ e
            Save.SaveIOError e     -> "Quickload failed: " ++ e
      modify (\s -> s { gsMessages = line : gsMessages s })

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
