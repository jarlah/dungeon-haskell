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

import Data.Char (ord)

import Game.AI.Runtime
  ( AIRuntime (..), AppEvent (..)
  , applyAIResponse, maybeFireGreeting, maybeFireQuest, maybeFireRoomDesc
  , startAIRuntime, stopAIRuntime
  )
import qualified Game.Audio as Audio
import qualified Game.Config as Config
import Game.GameState
import Game.Input (handleKey)
import Game.Logic.Command (Command(..), parseCommand, isCheatCommand)
import Game.Logic.Dungeon (defaultLevelConfig)
import Game.Logic.Quest (Quest(..), QuestStatus(..))
import Game.Render
  ( drawGame, bossAttr, doorAttr, fogAttr, npcAttr
  , saveMenuCursorAttr, saveMenuEmptyAttr
  , launchCursorAttr, launchDisabledAttr, launchTitleAttr
  )
import qualified Game.Save as Save
import Game.UI.Launch (handleLaunchMenuKey)
import Game.UI.SaveMenu (handleSaveMenuKey, openSaveMenu, sampleHasSaves)
import Game.UI.Types (Name (..), RuntimeFlags (..), parseArgs)
import Game.Types (Dir(..), GameAction(..), Inventory(..))

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

-- | Keystrokes while the victory modal is shown. The game is over
--   but not dead — any of q / Q / Esc opens the quit-confirmation
--   modal (so a fat-fingered key can't misfire), and everything
--   else is swallowed. There's no "continue playing" path because
--   the dragon is dead and the boss floor has no stairs down.
handleVictoryKey :: V.Key -> EventM Name GameState ()
handleVictoryKey (V.KChar 'q') =
  modify (\gs -> gs { gsConfirmQuit = True })
handleVictoryKey (V.KChar 'Q') =
  modify (\gs -> gs { gsConfirmQuit = True })
handleVictoryKey V.KEsc =
  modify (\gs -> gs { gsConfirmQuit = True })
handleVictoryKey _ = pure ()

-- | Keystrokes while the quit-confirmation modal is open.
--   @y@ actually halts the app; @n@, @Esc@, or anything else
--   closes the modal and returns to the game.
handleConfirmQuitKey :: V.Key -> EventM Name GameState ()
handleConfirmQuitKey (V.KChar 'y') = halt
handleConfirmQuitKey (V.KChar 'Y') = halt
handleConfirmQuitKey _ =
  modify (\gs -> gs { gsConfirmQuit = False })

-- | Keystrokes while the help modal is open. 'Esc' / 'q' / '?'
--   close it. Arrow keys, @j@ / @k@, PgUp / PgDn, Home / End drive
--   the scroll viewport so the reference sheet stays usable on
--   terminals that can't show the whole thing at once. Anything
--   else is swallowed so a stray keystroke doesn't accidentally
--   dismiss the modal mid-read.
handleHelpKey :: V.Key -> EventM Name GameState ()
handleHelpKey key = case key of
  V.KEsc        -> closeHelp
  V.KChar 'q'   -> closeHelp
  V.KChar '?'   -> closeHelp
  V.KUp         -> vScrollBy         helpVp (-1)
  V.KChar 'k'   -> vScrollBy         helpVp (-1)
  V.KDown       -> vScrollBy         helpVp 1
  V.KChar 'j'   -> vScrollBy         helpVp 1
  V.KPageUp     -> vScrollPage       helpVp Up
  V.KPageDown   -> vScrollPage       helpVp Down
  V.KHome       -> vScrollToBeginning helpVp
  V.KEnd        -> vScrollToEnd      helpVp
  _             -> pure ()
  where
    closeHelp = do
      vScrollToBeginning helpVp
      modify (\gs -> gs { gsHelpOpen = False })
    helpVp = viewportScroll HelpViewport

-- | Keystrokes while the quest log modal is open. Letters @a@..@z@
--   select the active quest at that index (the selection is shown
--   as an asterisk in the log); pressing @x@ while a selection is
--   in place abandons that quest (flipping it to QuestFailed).
--   Two keystrokes = built-in confirm. Esc or @j@ closes.
handleQuestLogKey :: V.Key -> EventM Name GameState ()
handleQuestLogKey V.KEsc =
  modify (\gs -> gs { gsQuestLogOpen = False, gsQuestLogCursor = Nothing })
handleQuestLogKey (V.KChar 'Q') =
  modify (\gs -> gs { gsQuestLogOpen = False, gsQuestLogCursor = Nothing })
handleQuestLogKey (V.KChar 'x') = do
  gs <- get
  case gsQuestLogCursor gs of
    Just idx -> modify (abandonQuest idx)
    Nothing  -> pure ()
handleQuestLogKey (V.KChar c)
  | c >= 'a' && c <= 'z' = do
      gs <- get
      let idx    = ord c - ord 'a'
          active = [ q | q <- gsQuests gs, qStatus q == QuestActive ]
      if idx < length active
        then modify (\s -> s { gsQuestLogCursor = Just idx })
        else pure ()
handleQuestLogKey _ = pure ()

-- | Keystrokes while an NPC dialogue modal is open. Lowercase
--   @a@..@z@ accept an offered quest; uppercase @A@..@Z@ hand in
--   a ready-to-turn-in quest the player is carrying; 'Esc' closes
--   the modal without any action. Monsters do not act either way
--   — peaceful conversation is a free action, as is collecting
--   quest rewards.
handleDialogueKey
  :: Maybe Audio.AudioSystem
  -> AIRuntime
  -> Int
  -> V.Key
  -> EventM Name GameState ()
handleDialogueKey _ _ _ V.KEsc =
  modify (\gs -> gs { gsDialogue = Nothing })
handleDialogueKey mAudio _ npcIdx (V.KChar c)
  | c >= 'a' && c <= 'z' = do
      gs <- get
      let offerIdx = ord c - ord 'a'
          offers   = case drop npcIdx (gsNPCs gs) of
            (n : _) -> npcOffers n
            []      -> []
      if offerIdx < length offers
        then do
          modify (acceptQuestFromNPC npcIdx offerIdx)
          -- If that was the last offer *and* the player has no
          -- ready quests to turn in here, auto-close the dialogue
          -- so they don't stare at an empty quest list.
          autoCloseIfIdle npcIdx
        else pure ()
  | c >= 'A' && c <= 'Z' = do
      gs <- get
      let readyIdx = ord c - ord 'A'
          ready    = [ q | q <- gsQuests gs, qStatus q == QuestReadyToTurnIn ]
      if readyIdx < length ready
        then do
          modify (turnInQuest npcIdx readyIdx)
          playEventsFor mAudio
          autoCloseIfIdle npcIdx
        else pure ()
handleDialogueKey _ _ _ _ = pure ()

-- | Close the dialogue modal if the NPC has no remaining offers
-- /and/ the player has no further quests ready to hand in here.
-- Called after accept/turn-in so the player isn't left staring at
-- an empty dialogue screen.
autoCloseIfIdle :: Int -> EventM Name GameState ()
autoCloseIfIdle npcIdx = do
  gs <- get
  let stillOffering = case drop npcIdx (gsNPCs gs) of
        (n : _) -> not (null (npcOffers n))
        []      -> False
      stillReady = any (\q -> qStatus q == QuestReadyToTurnIn) (gsQuests gs)
  if stillOffering || stillReady
    then pure ()
    else modify (\s -> s { gsDialogue = Nothing })

-- | Keystrokes while the game is waiting for a direction to
--   complete a two-step command (currently only close-door). The
--   eight movement keys resolve to a 'Dir' and dispatch the
--   pending 'DirectionalAction' through 'applyAction'; Esc or any
--   other key cancels with a "Never mind." message. Monsters are
--   only advanced on the successful-dispatch path — cancel and
--   invalid keys are free no-ops, same as bumping a wall.
handleAwaitingDirectionKey
  :: Maybe Audio.AudioSystem
  -> DirectionalAction
  -> V.Key
  -> EventM Name GameState ()
handleAwaitingDirectionKey mAudio dirAct key =
  case directionFromKey key of
    Just dir -> do
      modify $ \gs -> gs { gsAwaitingDirection = Nothing }
      let act = case dirAct of
            DirCloseDoor -> CloseDoor dir
      modify (applyAction act)
      playEventsFor mAudio
    Nothing  ->
      modify $ \gs -> gs
        { gsAwaitingDirection = Nothing
        , gsMessages          = "Never mind." : gsMessages gs
        }

-- | Map a movement key to a 'Dir'. Used by the directional-command
--   mode — the same keys that move the player in normal mode
--   supply the direction for the pending command.
directionFromKey :: V.Key -> Maybe Dir
directionFromKey key = case key of
  V.KUp       -> Just N
  V.KDown     -> Just S
  V.KLeft     -> Just W
  V.KRight    -> Just E
  V.KChar 'k' -> Just N
  V.KChar 'j' -> Just S
  V.KChar 'h' -> Just W
  V.KChar 'l' -> Just E
  V.KChar 'y' -> Just NW
  V.KChar 'u' -> Just NE
  V.KChar 'b' -> Just SW
  V.KChar 'n' -> Just SE
  _           -> Nothing

-- | Keystrokes while the inventory modal is open. Letters @a@..@z@
--   select the item at that index and apply its default action;
--   'Esc' or 'i' closes the modal without doing anything. Modal
--   actions still advance monsters via 'applyAction', so deciding
--   to swap gear mid-fight carries the usual tactical cost.
handleInventoryKey :: Maybe Audio.AudioSystem -> V.Key -> EventM Name GameState ()
handleInventoryKey _ V.KEsc =
  modify (\gs -> gs { gsInventoryOpen = False })
handleInventoryKey _ (V.KChar 'i') =
  modify (\gs -> gs { gsInventoryOpen = False })
handleInventoryKey mAudio (V.KChar c)
  | c >= 'a' && c <= 'z' = do
      gs <- get
      let idx = ord c - ord 'a'
      if idx < length (invItems (gsInventory gs))
        then do
          modify (\s -> s { gsInventoryOpen = False })
          modify (applyAction (UseItem idx))
          playEventsFor mAudio
        else pure ()
handleInventoryKey _ _ = pure ()

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

-- | Fire SFX for every GameEvent the last action produced.
--   Extracted so the inventory and normal paths share one place
--   that knows how to route events into the audio shell.
playEventsFor :: Maybe Audio.AudioSystem -> EventM Name GameState ()
playEventsFor Nothing      = pure ()
playEventsFor (Just audio) = do
  gs <- get
  liftIO $ mapM_ (Audio.playEvent audio) (gsEvents gs)

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
