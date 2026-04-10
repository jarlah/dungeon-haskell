{-# LANGUAGE DeriveGeneric #-}
module Game.State.Types
( GameState(..)
, LaunchOption(..)
, NPC (..)
, ParkedLevel (..)
, SaveMenu(..)
, SaveMenuMode(..)
, SaveMenuEntry(..)
, LaunchMenu(..)
, DirectionalAction(..)
, emit
) where

import Game.Types (KeyId, DungeonLevel, Monster, Item, Pos, Inventory, GameEvent, Stats)
import Game.Logic.Chest (Chest)
import Data.Set (Set)
import Game.Logic.Quest (Quest)
import qualified Game.Logic.Dungeon as D
import Game.Save.Types (SaveMetadata)
import Data.Map.Strict (Map)
import System.Random (StdGen)

-- | Pure snapshot of the whole game world.
data GameState = GameState
  { gsLevel       :: !DungeonLevel
  , gsPlayerPos   :: !Pos
  , gsPlayerStats :: !Stats
  , gsMonsters    :: ![Monster]
  , gsMessages    :: ![String]   -- ^ newest first
  , gsRng         :: !StdGen
  , gsDead        :: !Bool       -- ^ did the player die?
  , gsQuitting    :: !Bool
  , gsEvents      :: ![GameEvent]
    -- ^ events emitted during the most recent 'applyAction' call,
    --   in chronological order. Cleared at the start of each action.
  , gsVisible     :: !(Set Pos)
    -- ^ tiles currently in the player's field of view, recomputed
    --   at the end of every action.
  , gsExplored    :: !(Set Pos)
    -- ^ tiles the player has ever seen. Monotonically grows as the
    --   player explores; used to render a dim "fog of war" for
    --   tiles that are known but not currently visible.
  , gsPrompt      :: !(Maybe String)
    -- ^ slash-command prompt buffer. 'Nothing' means the prompt is
    --   closed and keystrokes drive the normal keymap; 'Just buf'
    --   means the prompt is open and keystrokes append to @buf@
    --   until 'Enter' submits or 'Esc' cancels.
  , gsInventory   :: !Inventory
    -- ^ what the player is carrying, plus equipped slots.
  , gsItemsOnFloor :: ![(Pos, Item)]
    -- ^ loot that has been dropped on the current level and not yet
    --   picked up. Kept as a flat list (duplicates allowed) so
    --   multiple items can pile on a single tile.
  , gsChests :: ![Chest]
    -- ^ respawning loot chests on the /current/ floor. Each chest
    --   is either 'ChestFull' (carrying one 'Item' the player can
    --   collect by bumping into it) or 'ChestEmpty' with a
    --   per-turn cooldown counter. The counter ticks down on every
    --   'tickPlayerTurn' call while the player is on this floor;
    --   the refill check happens in 'loadParked' on floor re-entry,
    --   so parked floors don't need to keep their own ticking
    --   clock. Placement is done by the generator
    --   ('newGame' / 'generateAndEnter') and stored on this field
    --   only — chests are /not/ a 'Tile' variant, so they don't
    --   interfere with existing terrain logic or the door /
    --   stairs path-finding.
  , gsInventoryOpen :: !Bool
    -- ^ is the inventory modal currently open? Input routes through
    --   the modal handler when true.
  , gsQuests      :: ![Quest]
    -- ^ quests the player has accepted. The list is small (2–3
    --   in the MVP) and we advance every quest with every event,
    --   so a flat list is fine. Completed/Failed quests stay in
    --   the list so the quest panel can show their final state;
    --   'advanceQuest' treats terminal statuses as absorbing.
    --
    --   Note: only *accepted* quests live here. Quests offered by
    --   NPCs but not yet accepted live on the NPC in 'gsNPCs'.
  , gsNPCs        :: ![NPC]
    -- ^ friendly non-combat entities on the current level. NPCs
    --   don't move, don't take turns, and bumping into them opens
    --   a dialogue modal instead of attacking.
  , gsDialogue    :: !(Maybe Int)
    -- ^ index into 'gsNPCs' of the NPC the player is currently
    --   talking to, or 'Nothing' when no dialogue is open. When
    --   this is 'Just', input routes through the dialogue handler
    --   and monsters do not act.
  , gsQuestLogOpen :: !Bool
    -- ^ is the quest log modal currently open?
  , gsQuestLogCursor :: !(Maybe Int)
    -- ^ index into the *active* quests in 'gsQuests' that the
    --   player has selected in the quest log, or 'Nothing' if no
    --   selection. Only meaningful when 'gsQuestLogOpen' is
    --   'True'. A selection exists so that pressing a letter
    --   (select) followed by @x@ (abandon) reads as a built-in
    --   two-step confirm.
  , gsConfirmQuit :: !Bool
    -- ^ is the quit-confirmation modal currently open? Set when
    --   the player presses @q@ / @Esc@ in normal mode so a
    --   fat-fingered quit key doesn't immediately end the run.
  , gsHelpOpen    :: !Bool
    -- ^ is the help modal currently open? The help modal lists
    --   every keybinding and slash command so the player doesn't
    --   have to remember them or scroll back through the README.
  , gsBossDepth   :: !Int
    -- ^ the depth at which the boss encounter lives, rolled once
    --   in 'newGame' from 'lcBossDepthRange' so every run has a
    --   single fixed boss floor. 'generateAndEnter' uses this to
    --   decide whether the next floor is the boss floor (strip
    --   'StairsDown', spawn a dragon in the last room, set
    --   'gsBossRoom'). Stored on the state so a return trip via
    --   up-stairs / down-stairs doesn't re-roll it.
  , gsBossRoom    :: !(Maybe D.Room)
    -- ^ the room containing the boss on the *currently loaded*
    --   level, or 'Nothing' on any non-boss floor. Set when
    --   'generateAndEnter' generates the boss floor; cleared when
    --   the player leaves it. Used by the music layer (M11f) to
    --   decide when to swap to boss music.
  , gsVictory     :: !Bool
    -- ^ has the player slain the boss? Set when the dragon dies
    --   and used by the render layer to show a victory modal and
    --   by the input layer to freeze gameplay the same way death
    --   does.
  , gsLevels      :: !(Map Int ParkedLevel)
    -- ^ previously-visited dungeon levels, keyed by their depth.
    --   The *current* level is always in 'gsLevel' and friends —
    --   parked entries only exist for floors the player has left
    --   but may return to. Each parked level remembers its
    --   monsters, items, explored set, and the position the player
    --   was standing on when they left, so going back up returns
    --   them to exactly where they descended from.
  , gsSaveMenu    :: !(Maybe SaveMenu)
    -- ^ state for the in-game save / load picker modal. 'Nothing'
    --   means the modal is closed and input routes normally.
    --   'Just' carries the mode (save vs load), the snapshotted
    --   list of save slots as of when the menu was opened, and
    --   the currently-highlighted cursor. The snapshot is taken
    --   when the menu opens so a save operation mid-menu doesn't
    --   cause the entry list to shift under the cursor.
  , gsLaunchMenu  :: !(Maybe LaunchMenu)
    -- ^ state for the title / launch screen shown at startup.
    --   'Just' means the player hasn't picked an entry point yet —
    --   input routes exclusively through the launch-menu handler
    --   and the renderer shows the title screen. The field is
    --   cleared when the player picks /New Game/, /Continue/, or
    --   /Load/. A successful load replaces the entire 'GameState',
    --   so the loaded blob — which was saved with 'Nothing' here —
    --   carries no leftover launch state.
  , gsRoomDesc    :: !(Maybe String)
    -- ^ Latest LLM-generated description for the room the player
    --   most recently walked into. 'Nothing' before the first
    --   reply has landed, or if AI is disabled. Replaced (not
    --   appended) when the player enters a new room so only the
    --   /current/ room's description is on screen.
  , gsRoomDescVisible :: !Bool
    -- ^ 'True' iff the description panel should currently be
    --   drawn. Set to 'True' when a new description arrives,
    --   flipped to 'False' by pressing Escape (panel dismiss).
    --   Independent from 'gsRoomDesc' so the player can dismiss a
    --   description and still have it re-appear on /next/ room
    --   entry without the old text flashing back first.
  , gsAwaitingDirection :: !(Maybe DirectionalAction)
    -- ^ Two-step input mode: when 'Just', the next keystroke is
    --   consumed as a direction and used to resolve the pending
    --   'DirectionalAction'. Set when the player presses a key
    --   that begins a directional command (currently only @c@
    --   for close-door); cleared when the direction is read (or
    --   the player cancels with Esc). Not meant to be persistent
    --   across saves — the save codec carries whatever value is
    --   current at 'encodeSave' time, but in practice the player
    --   saves outside any prompt.
  , gsCheatsUsed :: !Bool
    -- ^ 'True' if any wizard / cheat command has been invoked on
    --   this save at any point in its history. Once 'True', stays
    --   'True' — there is deliberately no way to unset it, so a
    --   save that was ever touched by @/heal@ or @/spawn@ can be
    --   cleanly distinguished from a save that was earned the
    --   hard way. Used by the launch / load menu to hide
    --   cheat-tainted saves from players running without the
    --   @--wizard@ flag, so the two histories don't mix. Stamped
    --   to 'True' inside 'applyCommand' before the command itself
    --   runs.
  , gsNextKeyId :: !Int
    -- ^ Monotonic counter for the next 'KeyId' to mint. Bumped
    --   every time the dungeon generator rolls a locked door, and
    --   every time a cross-level key gets scheduled into
    --   'gsPendingKeys'. Kept on the state (rather than global) so
    --   save/load round-trips preserve the naming — a key the
    --   player hasn't picked up yet keeps the same 'KeyId' and
    --   therefore the same display name after a load.
  , gsPendingKeys :: ![(Int, KeyId)]
    -- ^ Keys minted by a locked door on some floor that still
    --   need to be dropped as loot on a (possibly different)
    --   floor. Each entry is @(targetDepth, keyId)@; when the
    --   player first enters a floor, every pending entry whose
    --   depth matches is drained and placed as floor loot inside
    --   that floor's rooms. Scheduling is done at the moment a
    --   lock is created so a key is always guaranteed to exist on
    --   some reachable floor.
  , gsLockedDoorPrompt :: !(Maybe String)
    -- ^ 'Just name' when the player bumped a locked door without
    --   the matching key — the renderer shows a modal saying "this
    --   door needs the X key" and any keystroke clears it. The
    --   payload is the already-formatted key name so the modal
    --   doesn't have to reach back through a 'KeyId'. 'Nothing' in
    --   every other situation.
  , gsDashCooldown :: !Int
    -- ^ turns remaining before the player may 'Dash' again. 0
    --   means the dash is ready; every turn-advancing action
    --   decrements this toward 0 (see 'processMonsters'). Set to
    --   'dashCooldownTurns' when a dash is taken. Persisted in
    --   saves so the cooldown survives quicksave / quickload.
  , gsRegenCounter :: !Int
    -- ^ turns accumulated toward the next passive HP regen tick.
    --   Incremented every turn by 'tickRegen' as long as the
    --   player is at less than full HP AND no hostile monster is
    --   currently in the player's FOV. When it reaches
    --   'regenInterval' the player gains 1 HP (clamped to
    --   'sMaxHP') and the counter resets. Any turn where a
    --   hostile becomes visible — or the player takes damage
    --   while a hostile is visible — resets the counter to 0, so
    --   the player must fully disengage before sustain kicks in.
    --   This is the sustain reward for retreating via dash +
    --   close-door rather than tanking every encounter.
  , gsTurnsElapsed :: !Int
    -- ^ total turn-advancing actions the player has taken on
    --   /this/ run. Incremented once per 'tickPlayerTurn' call
    --   (so dash, wait, move, pickup, use, stairs all count —
    --   CloseDoor and blocked-move no-ops don't). The counter
    --   freezes on player death ('gsDead') and on victory (once
    --   'gsFinalTurns' has been snapshot), so the HUD and the
    --   victory modal can keep displaying the final number
    --   without it drifting forward while the player lingers on
    --   the end-of-run screen.
  , gsPotionsUsed :: !Int
    -- ^ total healing potions the player has quaffed on this run.
    --   Bumped in the 'IPotion' branch of 'playerUseItem' at the
    --   moment the potion is consumed. Used by the victory modal
    --   to show a "potions burned" line and factored into the
    --   run rank — a no-heal run ranks higher than a chug-and-win.
  , gsSavesUsed :: !Int
    -- ^ total successful save operations (quicksave + slot save)
    --   recorded on this run. Bumped at every save call site
    --   *before* the bytes hit disk, so the number persisted in
    --   the save blob already includes the save that's being
    --   written. Used by the victory modal / run rank as a
    --   self-regulating nudge: there is no punishment for saving,
    --   just a visible marker on the scoreboard.
  , gsFinalTurns :: !(Maybe Int)
    -- ^ snapshot of 'gsTurnsElapsed' at the moment the player won
    --   (the killing blow on the boss flips 'gsVictory' to
    --   'True'). 'Nothing' for an in-progress run. The HUD reads
    --   this in preference to the live counter once it becomes
    --   'Just' so the timer visibly freezes on victory instead of
    --   continuing to tick while the modal is open.
  } deriving (Show)

-- | Actions that need a direction supplied /after/ the initiating
--   keystroke. Stored in 'gsAwaitingDirection' while the game is
--   waiting on the second keystroke. Right now the only member is
--   close-door; kept as its own ADT so future directional commands
--   (kick, aim, shoot) can slot in without another GameState field.
data DirectionalAction
  = DirCloseDoor
  | DirFire
  deriving (Eq, Show)

-- | UI state for the save/load picker modal. Kept entirely in
--   pure 'GameState' so it round-trips through 'Binary' — the menu
--   itself is never meant to be present in an on-disk save, but
--   the whole 'GameState' is serialized as one blob and keeping
--   this field pure avoids a special-case handling of it. When a
--   save is loaded the menu field is cleared by the load handler.
data SaveMenu = SaveMenu
  { smMode    :: !SaveMenuMode
  , smSlots   :: ![SaveMenuEntry]
    -- ^ one entry per visible slot row, already collated from the
    --   live save directory at the moment the menu was opened and
    --   padded with placeholder rows for empty numbered slots so
    --   the player can write to them.
  , smCursor  :: !Int
    -- ^ index into 'smSlots' currently highlighted
  , smConfirm :: !Bool
    -- ^ 'True' iff we're waiting for a y/n on an overwrite
    --   confirmation (save mode only). The confirm prompt reuses
    --   the cursor row, so we only need a boolean flag here.
  } deriving (Eq, Show)

-- | Save vs. Load modal mode. The two share layout, cursor, and
--   entry list, but differ in what the letter keys do (write vs.
--   read) and in how empty rows render (writable placeholder vs.
--   greyed out).
data SaveMenuMode = SaveMode | LoadMode
  deriving (Eq, Show)

-- | One row in the save/load picker. Rows cover both /existing/
--   saves and /empty/ numbered slots the player may write to, so
--   the menu is a stable grid rather than a collapsing list.
data SaveMenuEntry = SaveMenuEntry
  { sseMeta :: !(Maybe SaveMetadata)
    -- ^ 'Just' when a save file already exists at this slot,
    --   'Nothing' for an empty slot (still selectable in save
    --   mode, greyed-out in load mode).
  , sseSlotLabel :: !String
    -- ^ pre-rendered slot identifier ("Quick", "Slot 1", ...)
    --   so the renderer doesn't need to pattern-match on the
    --   underlying 'SaveSlot' type.
  , sseIsQuick :: !Bool
    -- ^ 'True' for the quicksave row, 'False' for numbered slots
  , sseSlotNum :: !Int
    -- ^ 0 for the quick slot, N for NumberedSlot N
  } deriving (Eq, Show)

-- | UI state for the launch / title screen shown at startup.
--   The options themselves live in 'launchOptions' so the renderer
--   and the input handler agree on the order. Only the cursor
--   position is stored here — everything else is derived.
data LaunchMenu = LaunchMenu
  { lmCursor   :: !Int
    -- ^ index into 'launchOptions' currently highlighted
  , lmHasSaves :: !Bool
    -- ^ sampled from the save directory when the launch screen
    --   was opened. 'False' greys out the /Continue/ and /Load/
    --   options in the renderer, and the handler refuses to act
    --   on them — both keep the menu open with no side effect.
  } deriving (Eq, Show)

-- | Options shown on the launch screen. The order here is the
--   on-screen order and the index used by 'lmCursor'.
data LaunchOption
  = LaunchNewGame
    -- ^ start a fresh run
  | LaunchContinue
    -- ^ load the most recent save (quicksave or slot) — disabled
    --   when no saves exist
  | LaunchLoad
    -- ^ open the load picker to choose a specific slot — disabled
    --   when no saves exist
  | LaunchQuit
    -- ^ exit the game without playing
  deriving (Eq, Show)

-- | State of a dungeon level the player is not currently standing
--   on. Preserves everything that should survive a round-trip
--   through the stairs: map layout, unkilled monsters, unlooted
--   items, explored tiles, and where the player last stood.
data ParkedLevel = ParkedLevel
  { plLevel     :: !DungeonLevel
  , plMonsters  :: ![Monster]
  , plItems     :: ![(Pos, Item)]
  , plChests    :: ![Chest]
    -- ^ chests on this floor at the moment it was parked. Ticks
    --   only run for chests on the /current/ floor via
    --   'tickPlayerTurn', so parked chests pass their dormant time
    --   frozen and are refilled on re-entry in 'loadParked'.
  , plExplored  :: !(Set Pos)
  , plPlayerPos :: !Pos
  , plBossRoom  :: !(Maybe D.Room)
    -- ^ the boss room on this level if any. Non-boss floors carry
    --   'Nothing'. Stashed so a round-trip via stairs preserves
    --   which room on the boss floor is the dragon's lair.
  } deriving (Show)

-- | A friendly non-combat entity sitting on the dungeon floor.
--   NPCs give out quests via a dialogue modal. They don't take
--   turns, don't move, and can't be attacked — bumping into them
--   opens dialogue instead.
data NPC = NPC
  { npcName     :: !String
    -- ^ display name shown in the dialogue header
  , npcPos      :: !Pos
  , npcGreeting :: !String
    -- ^ hardcoded fallback greeting. Always present so the dialogue
    --   is displayable even with AI disabled, the backend down, or
    --   the response still in flight.
  , npcAIGreet  :: !(Maybe String)
    -- ^ LLM-generated replacement greeting, cached per NPC. The
    --   render layer prefers this over 'npcGreeting' whenever it's
    --   'Just', so a single successful AI reply sticks for the rest
    --   of the session. 'Nothing' means the request hasn't been
    --   fired yet, is still in flight, or failed — in any of those
    --   cases we fall back to 'npcGreeting'.
  , npcOffers   :: ![Quest]
    -- ^ quests the NPC has to give. Each entry has status
    --   'QuestNotStarted'; accepting a quest removes it from this
    --   list and moves it into 'gsQuests' with status
    --   'QuestActive'. Rejecting (Esc-ing out of dialogue) leaves
    --   it here so the player can come back later.
  } deriving (Eq, Show)
  
-- | Append events to the running per-turn log.
emit :: GameState -> [GameEvent] -> GameState
emit gs evs = gs { gsEvents = gsEvents gs ++ evs }