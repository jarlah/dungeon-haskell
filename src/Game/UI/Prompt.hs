-- | Slash-command prompt and command dispatch, extracted from
--   @app/Main.hs@.
--
--   This module owns two things:
--
--   * 'handlePromptKey' / 'dispatchCommand' — the small state
--     machine that eats keystrokes while the @/command@ prompt is
--     open, parses the buffer on Enter, and routes the resulting
--     'Command' to the right effect.
--   * 'doQuicksave' / 'doQuickload' — the free-action quick-slot
--     helpers. They live here (rather than in the normal-mode
--     handler) because 'dispatchCommand' needs to call them for
--     the @/quicksave@ and @/quickload@ commands, and the
--     normal-mode @F5@ / @F9@ bindings in "Main" can import them
--     from the same place.
module Game.UI.Prompt
  ( handlePromptKey
  , dispatchCommand
  , doQuicksave
  , doQuickload
    -- * Pure helpers (exposed for testing)
  , PromptStep (..)
  , stepPromptBuffer
  , CommandEffect (..)
  , routeCommand
  ) where

import Brick (EventM, get, modify, put)
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V

import qualified Game.Audio as Audio
import Game.Core
import Game.Logic.Command (Command (..), isCheatCommand, parseCommand)
import qualified Game.Save as Save
import Game.Types (GameAction (..))
import Game.UI.Modals (playEventsFor)
import Game.UI.SaveMenu (openSaveMenu)
import Game.UI.Types (Name, RuntimeFlags (..))

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
handlePromptKey mAudio rFlags key buf = case stepPromptBuffer key buf of
  PromptCancel ->
    modify (\gs -> gs { gsPrompt = Nothing })
  PromptSubmit submitted -> do
    modify (\gs -> gs { gsPrompt = Nothing })
    case parseCommand submitted of
      Right cmd -> dispatchCommand mAudio rFlags cmd
      Left err ->
        modify (\gs -> gs { gsMessages = ("Error: " ++ err) : gsMessages gs })
  PromptEdit buf' ->
    modify (\gs -> gs { gsPrompt = Just buf' })
  PromptIgnore ->
    pure ()

-- | Pure state-machine step for the slash-command prompt buffer.
--   Given the pressed key and the current buffer, decides what
--   should happen next: close the prompt ('PromptCancel'), submit
--   the buffer ('PromptSubmit'), replace the buffer with a new
--   edited version ('PromptEdit'), or do nothing ('PromptIgnore').
--
--   Extracted from 'handlePromptKey' so the per-keystroke decision
--   table can be unit-tested without a 'Brick' runtime.
data PromptStep
  = PromptCancel
    -- ^ Esc — close the prompt without dispatching anything.
  | PromptSubmit String
    -- ^ Enter — close the prompt and submit this buffer.
  | PromptEdit String
    -- ^ Backspace / printable character — replace the buffer with
    --   this new value and keep the prompt open.
  | PromptIgnore
    -- ^ Any other key — no state change.
  deriving (Eq, Show)

-- | The decision function that drives the prompt state machine.
--   Pure — the effect of each step is applied by 'handlePromptKey'.
stepPromptBuffer :: V.Key -> String -> PromptStep
stepPromptBuffer V.KEsc       _   = PromptCancel
stepPromptBuffer V.KEnter     buf = PromptSubmit buf
stepPromptBuffer V.KBS        buf = PromptEdit (dropLast buf)
stepPromptBuffer (V.KChar c)  buf = PromptEdit (buf ++ [c])
stepPromptBuffer _            _   = PromptIgnore

dropLast :: [a] -> [a]
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
dispatchCommand mAudio rFlags cmd = case routeCommand rFlags cmd of
  EffSetHelp ->
    modify (\gs -> gs { gsHelpOpen = True })
  EffConfirmQuit ->
    modify (\gs -> gs { gsConfirmQuit = True })
  EffSetInventory ->
    modify (\gs -> gs { gsInventoryOpen = True })
  EffSetQuestLog ->
    modify $ \gs -> gs
      { gsQuestLogOpen   = True
      , gsQuestLogCursor = Nothing
      }
  EffApplyWait -> do
    modify (applyAction Wait)
    playEventsFor mAudio
  EffSaveMenu mode -> openSaveMenu rFlags mode
  EffQuicksave     -> doQuicksave
  EffQuickload     -> doQuickload
  EffOpenMixer ->
    modify (\gs -> gs { gsVolumeMixer = Just VolumeMixer { vmCursor = VolMusic } })
  EffCheatBlocked ->
    modify $ \gs -> gs
      { gsMessages =
          "Cheats are disabled. Launch with --wizard to enable."
            : gsMessages gs
      }
  EffApplyCommand c -> modify (applyCommand c)

-- | The set of effects a parsed 'Command' can resolve to. Pure
--   classification of what 'dispatchCommand' should do, decoupled
--   from the 'Brick' runtime so the wizard-gating rule can be
--   unit-tested directly.
data CommandEffect
  = EffSetHelp
    -- ^ Open the help modal.
  | EffConfirmQuit
    -- ^ Open the quit-confirmation modal.
  | EffSetInventory
    -- ^ Open the inventory modal.
  | EffSetQuestLog
    -- ^ Open the quest log modal (with no initial cursor).
  | EffApplyWait
    -- ^ Apply a 'Wait' action and advance monsters.
  | EffSaveMenu SaveMenuMode
    -- ^ Open the save picker in the given mode.
  | EffQuicksave
    -- ^ Free-action quicksave.
  | EffQuickload
    -- ^ Free-action quickload.
  | EffOpenMixer
    -- ^ Open the volume mixer modal.
  | EffCheatBlocked
    -- ^ The command is a cheat and @--wizard@ was not passed —
    --   report the refusal in the message log.
  | EffApplyCommand Command
    -- ^ Fall through to 'applyCommand' for this command.
  deriving (Eq, Show)

-- | Pure classifier for dispatch. Safe UI commands resolve to
--   dedicated effects; cheat commands are either blocked (no
--   @--wizard@) or passed through to 'applyCommand'.
routeCommand :: RuntimeFlags -> Command -> CommandEffect
routeCommand rFlags cmd = case cmd of
  CmdHelp       -> EffSetHelp
  CmdQuit       -> EffConfirmQuit
  CmdInventory  -> EffSetInventory
  CmdQuests     -> EffSetQuestLog
  CmdWait       -> EffApplyWait
  CmdSave       -> EffSaveMenu SaveMode
  CmdLoad       -> EffSaveMenu LoadMode
  CmdQuicksave  -> EffQuicksave
  CmdQuickload  -> EffQuickload
  CmdVolume     -> EffOpenMixer
  _
    | isCheatCommand cmd, not (rfWizardEnabled rFlags) -> EffCheatBlocked
    | otherwise -> EffApplyCommand cmd

-- | Save the current 'GameState' to the quicksave slot and report
--   the outcome into the message log. Quicksave is a free action —
--   it does not emit game events, does not clear the event queue,
--   and does not advance monsters. On failure the game continues
--   untouched with an error line in the log so the player can see
--   what went wrong.
doQuicksave :: EventM Name GameState ()
doQuicksave = do
  gs0 <- get
  -- Build the would-be-saved state with the counter bumped, so
  -- the blob on disk already reflects this save — loading it
  -- back gives the same counter as "kept running". Only commit
  -- the bump to the live state if the write actually succeeds;
  -- a failed save shouldn't nudge the run rank.
  let gs = gs0 { gsSavesUsed = gsSavesUsed gs0 + 1 }
  res <- liftIO (Save.writeSave Save.QuickSlot gs)
  let line = case res of
        Right ()                   -> "Quicksaved."
        Left Save.SaveMissing      -> "Quicksave failed: save directory missing."
        Left Save.SaveWrongMagic   -> "Quicksave failed: internal error (magic)."
        Left Save.SaveWrongVersion -> "Quicksave failed: internal error (version)."
        Left (Save.SaveCorrupt e)  -> "Quicksave failed: " ++ e
        Left (Save.SaveIOError e)  -> "Quicksave failed: " ++ e
  case res of
    Right () -> put gs { gsMessages = line : gsMessages gs }
    Left _   -> modify (\s -> s { gsMessages = line : gsMessages s })

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
