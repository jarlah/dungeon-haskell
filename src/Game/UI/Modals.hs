-- | Modal key handlers, extracted from @app/Main.hs@.
--
--   This module owns every handler that fires while the game is
--   showing something /over/ the main map: the victory screen, the
--   quit-confirmation dialog, the scrollable help sheet, the quest
--   log, NPC dialogues, the inventory picker, and the two-step
--   \"close door in which direction?\" prompt.
--
--   'playEventsFor' lives here too because the dialogue, inventory
--   and directional handlers all need to route game events into
--   the audio shell, and wiring it through a function argument was
--   more fuss than it was worth. The normal-mode handler imports
--   the same helper from this module.
module Game.UI.Modals
  ( -- * Modal handlers
    handleVictoryKey
  , handleConfirmQuitKey
  , handleHelpKey
  , handleQuestLogKey
  , handleDialogueKey
  , autoCloseIfIdle
  , handleAwaitingDirectionKey
  , directionFromKey
  , handleInventoryKey
    -- * Shared audio helper
  , playEventsFor
    -- * Pure helpers (exposed for testing)
  , stepAutoClose
  , stepDialogueAccept
  , stepDialogueTurnIn
  , stepQuestLogSelect
  ) where

import Brick
  ( EventM, Direction (..), get, halt, modify, put
  , vScrollBy, vScrollPage, vScrollToBeginning, vScrollToEnd, viewportScroll
  )
import Control.Monad.IO.Class (liftIO)
import Data.Char (ord)
import qualified Graphics.Vty as V

import Game.AI.Runtime (AIRuntime)
import qualified Game.Audio as Audio
import Game.GameState
import Game.Logic.Quest (Quest (..), QuestStatus (..))
import Game.Types (Dir (..), GameAction (..), Inventory (..))
import Game.UI.Types (Name (..))

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
  | c >= 'a' && c <= 'z' = modify (stepQuestLogSelect c)
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
      case stepDialogueAccept npcIdx c gs of
        Just gs' -> do
          put gs'
          -- If that was the last offer *and* the player has no
          -- ready quests to turn in here, auto-close the dialogue
          -- so they don't stare at an empty quest list.
          autoCloseIfIdle npcIdx
        Nothing  -> pure ()
  | c >= 'A' && c <= 'Z' = do
      gs <- get
      case stepDialogueTurnIn npcIdx c gs of
        Just gs' -> do
          put gs'
          playEventsFor mAudio
          autoCloseIfIdle npcIdx
        Nothing  -> pure ()
handleDialogueKey _ _ _ _ = pure ()

-- | Close the dialogue modal if the NPC has no remaining offers
-- /and/ the player has no further quests ready to hand in here.
-- Called after accept/turn-in so the player isn't left staring at
-- an empty dialogue screen.
autoCloseIfIdle :: Int -> EventM Name GameState ()
autoCloseIfIdle npcIdx = modify (stepAutoClose npcIdx)

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

-- | Fire SFX for every GameEvent the last action produced.
--   Extracted so the inventory and normal paths share one place
--   that knows how to route events into the audio shell.
playEventsFor :: Maybe Audio.AudioSystem -> EventM Name GameState ()
playEventsFor Nothing      = pure ()
playEventsFor (Just audio) = do
  gs <- get
  liftIO $ mapM_ (Audio.playEvent audio) (gsEvents gs)

--------------------------------------------------------------------
-- Pure helpers
--------------------------------------------------------------------

-- | Pure core of 'autoCloseIfIdle': clear 'gsDialogue' if the NPC
--   at the given index has no remaining offers and the player has
--   no quests ready to turn in. Otherwise return the state
--   unchanged.
stepAutoClose :: Int -> GameState -> GameState
stepAutoClose npcIdx gs
  | stillOffering || stillReady = gs
  | otherwise                   = gs { gsDialogue = Nothing }
  where
    stillOffering = case drop npcIdx (gsNPCs gs) of
      (n : _) -> not (null (npcOffers n))
      []      -> False
    stillReady = any (\q -> qStatus q == QuestReadyToTurnIn) (gsQuests gs)

-- | Pure core of the lowercase @a@..@z@ branch in
--   'handleDialogueKey'. Given an NPC index and the accept letter,
--   returns the new state after accepting the offer at that index,
--   or 'Nothing' if the letter points past the end of the offer
--   list. The caller is responsible for the subsequent auto-close
--   check.
stepDialogueAccept :: Int -> Char -> GameState -> Maybe GameState
stepDialogueAccept npcIdx c gs
  | offerIdx < length offers = Just (acceptQuestFromNPC npcIdx offerIdx gs)
  | otherwise                = Nothing
  where
    offerIdx = ord c - ord 'a'
    offers   = case drop npcIdx (gsNPCs gs) of
      (n : _) -> npcOffers n
      []      -> []

-- | Pure core of the uppercase @A@..@Z@ branch in
--   'handleDialogueKey'. Given an NPC index and the turn-in
--   letter, returns the new state after handing in the ready-quest
--   at that index, or 'Nothing' if the letter points past the end
--   of the ready-to-turn-in list.
stepDialogueTurnIn :: Int -> Char -> GameState -> Maybe GameState
stepDialogueTurnIn npcIdx c gs
  | readyIdx < length ready = Just (turnInQuest npcIdx readyIdx gs)
  | otherwise               = Nothing
  where
    readyIdx = ord c - ord 'A'
    ready    = [ q | q <- gsQuests gs, qStatus q == QuestReadyToTurnIn ]

-- | Pure core of the letter branch in 'handleQuestLogKey'. Given
--   an @a@..@z@ key and the current state, set the quest-log
--   cursor to that index if it points at an active quest.
--   Out-of-range indices leave the cursor unchanged.
stepQuestLogSelect :: Char -> GameState -> GameState
stepQuestLogSelect c gs
  | idx < length active = gs { gsQuestLogCursor = Just idx }
  | otherwise           = gs
  where
    idx    = ord c - ord 'a'
    active = [ q | q <- gsQuests gs, qStatus q == QuestActive ]
