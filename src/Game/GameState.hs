module Game.GameState
  ( GameState(..)
  , DirectionalAction(..)
  , ParkedLevel(..)
  , NPC(..)
  , SaveMenu(..)
  , SaveMenuMode(..)
  , SaveMenuEntry(..)
  , LaunchMenu(..)
  , LaunchOption(..)
  , launchOptions
  , mkGameState
  , newGame
  , defaultPlayerStats
  , hardcodedRoom
  , hardcodedInitialState
  , applyAction
  , applyCommand
  , acceptQuestFromNPC
  , abandonQuest
  , turnInQuest
  , shouldPlayBossMusic
  , fovRadius
  , monsterSightRadius
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Vector as V
import Linear (V2(..))
import System.Random (StdGen, mkStdGen, randomR)

import Game.Save.Types (SaveMetadata)
import Game.Types
import qualified Game.Logic.Combat as C
import Game.Logic.Combat (Damage(..))
import Game.Logic.Command (Command(..), isCheatCommand)
import qualified Game.Logic.Dungeon as D
import qualified Game.Logic.FOV as FOV
import qualified Game.Logic.Inventory as Inv
import qualified Game.Logic.Loot as Loot
import Game.Logic.MonsterAI (MonsterIntent(..), monsterIntent)
import qualified Game.Logic.Movement as M
import qualified Game.Logic.Progression as P
import Game.Logic.Quest
  ( Quest(..), QuestEvent(..), QuestGoal(..), QuestStatus(..)
  , advanceAll, isReady, mkQuest
  )

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
  } deriving (Show)

-- | Actions that need a direction supplied /after/ the initiating
--   keystroke. Stored in 'gsAwaitingDirection' while the game is
--   waiting on the second keystroke. Right now the only member is
--   close-door; kept as its own ADT so future directional commands
--   (kick, aim, shoot) can slot in without another GameState field.
data DirectionalAction
  = DirCloseDoor
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

-- | The fixed order of options on the launch screen. Kept as a
--   top-level list so the renderer and the key handler agree on
--   indices without having to duplicate the list.
launchOptions :: [LaunchOption]
launchOptions = [LaunchNewGame, LaunchContinue, LaunchLoad, LaunchQuit]

-- | State of a dungeon level the player is not currently standing
--   on. Preserves everything that should survive a round-trip
--   through the stairs: map layout, unkilled monsters, unlooted
--   items, explored tiles, and where the player last stood.
data ParkedLevel = ParkedLevel
  { plLevel     :: !DungeonLevel
  , plMonsters  :: ![Monster]
  , plItems     :: ![(Pos, Item)]
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

-- | How far the player can see, in tiles. Measured in Euclidean
--   distance; 8 feels right for a 60×20 dungeon.
fovRadius :: Int
fovRadius = 8

-- | How far a monster can see, in tiles. Kept symmetric with
--   'fovRadius' so "if I can see it, it can see me" — Milestone 16
--   can retune if playtesting shows the player needs a scouting
--   advantage. Measured in Euclidean distance, matching the FOV.
monsterSightRadius :: Int
monsterSightRadius = 8

defaultPlayerStats :: Stats
defaultPlayerStats = Stats
  { sHP      = 20
  , sMaxHP   = 20
  , sAttack  = 6
  , sDefense = 2
  , sSpeed   = 10
  , sLevel   = 1
  , sXP      = 0
  }

-- | Construct a 'GameState' from the given parts.
mkGameState :: StdGen -> DungeonLevel -> Pos -> [Monster] -> GameState
mkGameState gen dl start monsters = recomputeVisibility GameState
  { gsLevel       = dl
  , gsPlayerPos   = start
  , gsPlayerStats = defaultPlayerStats
  , gsMonsters    = monsters
  , gsMessages    = ["Welcome to the dungeon!"]
  , gsRng         = gen
  , gsDead        = False
  , gsQuitting    = False
  , gsEvents      = []
  , gsVisible     = Set.empty
  , gsExplored    = Set.empty
  , gsPrompt      = Nothing
  , gsInventory   = emptyInventory
  , gsItemsOnFloor = []
  , gsInventoryOpen = False
  , gsQuests      = []
  , gsNPCs        = []
  , gsDialogue    = Nothing
  , gsQuestLogOpen   = False
  , gsQuestLogCursor = Nothing
  , gsConfirmQuit    = False
  , gsHelpOpen       = False
  , gsBossDepth      = 10
  , gsBossRoom       = Nothing
  , gsVictory        = False
  , gsLevels         = Map.empty
  , gsSaveMenu       = Nothing
  , gsLaunchMenu     = Nothing
  , gsRoomDesc       = Nothing
  , gsRoomDescVisible = False
  , gsAwaitingDirection = Nothing
  , gsCheatsUsed        = False
  , gsNextKeyId         = 0
  , gsPendingKeys       = []
  , gsLockedDoorPrompt  = Nothing
  }

-- | Build a quest as an un-accepted *offer*. Same as 'mkQuest' but
--   with status 'QuestNotStarted' so 'advanceQuest' will ignore it
--   until the player accepts — at which point the status flips to
--   'QuestActive' via 'acceptOffer'.
mkOffer :: String -> QuestGoal -> Quest
mkOffer name goal = (mkQuest name goal) { qStatus = QuestNotStarted }

-- | Flip an offered quest into an accepted one.
acceptOffer :: Quest -> Quest
acceptOffer q = q { qStatus = QuestActive }

-- | Refresh 'gsVisible' from the player's current position and fold
--   the new FOV into 'gsExplored'. Called once at the end of every
--   action so the rendering layer always has up-to-date sets.
recomputeVisibility :: GameState -> GameState
recomputeVisibility gs =
  let vis = FOV.computeFOV (gsLevel gs) (gsPlayerPos gs) fovRadius
  in gs { gsVisible  = vis
        , gsExplored = Set.union (gsExplored gs) vis
        }

-- | Create a fresh game: roll the boss depth, generate the starting
--   level, spawn monsters and NPCs, build state. Depth 1 is never a
--   boss floor (the range lives well below that) so 'newGame' uses
--   the plain-floor path unconditionally.
newGame :: StdGen -> D.LevelConfig -> GameState
newGame gen0 cfg =
  let (bossDepth, gen1)                                = randomR (D.lcBossDepthRange cfg) gen0
      (dl, startPos, rooms, mLocked, nextKey, gen2)    = D.generateLevel gen1 cfg 0
      -- Don't spawn monsters in the player's starting room.
      spawnRooms       = drop 1 rooms
      (monsters, gen3) = spawnMonsters gen2 spawnRooms
      (npcs,     gen4) = spawnNPCs gen3 (D.lcDepth cfg) rooms
      -- If this floor minted a lock, the matching key is placed
      -- immediately on this same floor, inside the spawn-side
      -- component of the lock — so the player can always reach the
      -- key before ever needing to open the door.
      reachable        = spawnSideReachable dl startPos
      keysToPlace      = case mLocked of
        Just (kid, _) -> [kid]
        Nothing       -> []
      (keyLoot, gen5)  = placeKeyLoot gen4 reachable rooms keysToPlace
      base             = mkGameState gen5 dl startPos monsters
  in base
       { gsNPCs         = npcs
       , gsBossDepth    = bossDepth
       , gsNextKeyId    = nextKey
       , gsPendingKeys  = []
       , gsItemsOnFloor = keyLoot
       }

-- | 4-connected flood fill from the player spawn, treating walls
--   AND any locked door as impassable (but treating closed doors as
--   passable, since bump-to-open doesn't need a key). The result is
--   exactly the set of tiles a keyless player can reach from their
--   spawn tile. At most one locked door exists per level, so this is
--   equivalent to "reachable while the lock stays locked."
--
--   Used by 'placeKeyLoot' to guarantee a minted key is never placed
--   on the far side of its own lock (which would softlock the run).
spawnSideReachable :: DungeonLevel -> Pos -> Set Pos
spawnSideReachable dl start = go (Set.singleton start) [start]
  where
    passable p = case tileAt dl p of
      Just Wall           -> False
      Just (Door (Locked _)) -> False
      Just _              -> True
      Nothing             -> False

    go visited []       = visited
    go visited (p : qs) =
      let neighbors =
            [ p + V2 0 (-1)
            , p + V2 0   1
            , p + V2 1   0
            , p + V2 (-1) 0
            ]
          fresh =
            [ n
            | n <- neighbors
            , not (Set.member n visited)
            , passable n
            ]
      in go (foldr Set.insert visited fresh) (qs ++ fresh)

-- | Place NPCs for a freshly generated level. For the M10.1 MVP
--   this only fires on depth 1 and drops a single "Quest Master"
--   NPC in a non-starting room carrying the two MVP quests.
spawnNPCs :: StdGen -> Int -> [D.Room] -> ([NPC], StdGen)
spawnNPCs gen depth rooms
  | depth /= 1 = ([], gen)
  | otherwise  = case drop 1 rooms of
      []        -> ([], gen)                 -- degenerate: only one room
      (r : _)   ->
        let (p, gen') = randomRoomPos r gen
            questMaster = NPC
              { npcName     = "Quest Master"
              , npcPos      = p
              , npcGreeting = "Greetings, traveler. I have work for those willing."
              , npcAIGreet  = Nothing
              , npcOffers   =
                  [ (mkOffer "Slayer"        (GoalKillMonsters 5)) { qReward = 50 }
                  , (mkOffer "Delve"         (GoalReachDepth 3))   { qReward = 75 }
                  , (mkOffer "Slay the Dragon" GoalKillBoss)       { qReward = 500 }
                  ]
              }
        in ([questMaster], gen')

-- | Roll 0-2 monsters per candidate room and drop them in random spots.
spawnMonsters :: StdGen -> [D.Room] -> ([Monster], StdGen)
spawnMonsters gen0 = foldl' step ([], gen0)
  where
    step (acc, gen) r =
      let (count, g1) = randomR (0 :: Int, 2) gen
          (ms,    g2) = spawnInRoom g1 r count
      in (acc ++ ms, g2)

spawnInRoom :: StdGen -> D.Room -> Int -> ([Monster], StdGen)
spawnInRoom gen0 r n
  | n <= 0    = ([], gen0)
  | otherwise =
      let (kind, g1) = randomMonsterKind gen0
          (p,    g2) = randomRoomPos r g1
          m          = mkMonster kind p
          (rest, g3) = spawnInRoom g2 r (n - 1)
      in (m : rest, g3)

-- | Pick a random non-boss monster kind with uniform weight.
--   Bosses are placed by 'spawnBoss', not by the regular random
--   roll, so this explicitly enumerates the small monsters.
randomMonsterKind :: StdGen -> (MonsterKind, StdGen)
randomMonsterKind gen0 =
  let (i, gen1) = randomR (0 :: Int, 2) gen0
      k = case i of
            0 -> Rat
            1 -> Goblin
            _ -> Orc
  in (k, gen1)

randomRoomPos :: D.Room -> StdGen -> (Pos, StdGen)
randomRoomPos r gen0 =
  let (px, g1) = randomR (D.rX r, D.rX r + D.rW r - 1) gen0
      (py, g2) = randomR (D.rY r, D.rY r + D.rH r - 1) g1
  in (V2 px py, g2)

-- | A hardcoded 20x10 room (Milestone 1 fixture).
hardcodedRoom :: DungeonLevel
hardcodedRoom = DungeonLevel
  { dlWidth  = 20
  , dlHeight = 10
  , dlDepth  = 1
  , dlTiles  = V.generate (20 * 10) mkTile
  , dlRooms  = [Room 1 1 18 8]
  }
  where
    mkTile i =
      let (y, x) = i `divMod` 20
      in if x == 0 || y == 0 || x == 19 || y == 9
           then Wall
           else Floor

hardcodedInitialState :: GameState
hardcodedInitialState = mkGameState (mkStdGen 0) hardcodedRoom (V2 5 5) []

------------------------------------------------------------
-- Action processing
------------------------------------------------------------

-- | Apply a parsed slash-command. Only the wizard / debug helpers
--   are handled here — they do not cost a turn, do not emit game
--   events, and do not advance monsters. Safe UI commands (@/help@,
--   @/save@, ...) are dispatched inline by the prompt handler
--   instead, because several of them need IO.
--
--   Every call stamps 'gsCheatsUsed' so saves written after a
--   cheat command can never be mistaken for a clean run. The stamp
--   is permanent — there is no way to unset it.
--
--   Safe commands that somehow end up here (a bug) become no-ops
--   with a log line instead of crashing, so the parser and the
--   dispatcher stay loosely coupled.
--
--   Each effect is wrapped in 'recomputeVisibility' so anything
--   that moves the player or reshapes the level leaves 'gsVisible'
--   / 'gsExplored' consistent.
applyCommand :: Command -> GameState -> GameState
applyCommand cmd gs =
  let gsMarked
        | isCheatCommand cmd = gs { gsCheatsUsed = True }
        | otherwise          = gs
  in recomputeVisibility $ case cmd of
       CmdReveal       -> wizCmdReveal gsMarked
       CmdHeal         -> wizCmdHeal gsMarked
       CmdKillAll      -> wizCmdKillAll gsMarked
       CmdTeleport p   -> wizCmdTeleport p gsMarked
       CmdSpawn k      -> wizCmdSpawn k gsMarked
       CmdXP n         -> wizCmdXP n gsMarked
       CmdDescend      -> wizCmdDescend gsMarked
       CmdAscend       -> wizCmdAscend gsMarked
       -- Safe commands are dispatched inline by the prompt handler;
       -- reaching 'applyCommand' with one is a wiring bug. Log it
       -- and leave the state alone rather than crashing.
       _ -> gsMarked
              { gsMessages =
                  ("Internal: safe command routed to applyCommand")
                    : gsMessages gsMarked
              }

-- | Prepend a wizard-flavored log line.
wizMsg :: String -> GameState -> GameState
wizMsg m gs = gs { gsMessages = ("Wizard: " ++ m) : gsMessages gs }

wizCmdReveal :: GameState -> GameState
wizCmdReveal gs =
  let dl   = gsLevel gs
      all_ = Set.fromList
        [ V2 x y
        | x <- [0 .. dlWidth  dl - 1]
        , y <- [0 .. dlHeight dl - 1]
        ]
  in wizMsg "map revealed." gs { gsExplored = Set.union (gsExplored gs) all_ }

wizCmdHeal :: GameState -> GameState
wizCmdHeal gs =
  let s  = gsPlayerStats gs
      s' = s { sHP = sMaxHP s }
  in wizMsg "fully healed." gs { gsPlayerStats = s' }

wizCmdKillAll :: GameState -> GameState
wizCmdKillAll gs =
  let n = length (gsMonsters gs)
  in wizMsg (show n ++ " monster(s) banished.") gs { gsMonsters = [] }

-- | Teleport if the target tile is walkable *and* in-bounds.
--   Refuses silently-with-message otherwise so the wizard doesn't
--   phase into a wall or off the edge of the map.
wizCmdTeleport :: Pos -> GameState -> GameState
wizCmdTeleport p gs =
  case tileAt (gsLevel gs) p of
    Just t | isWalkable t ->
      wizMsg ("teleported to " ++ showPos p ++ ".") gs { gsPlayerPos = p }
    Just _ ->
      wizMsg ("tile at " ++ showPos p ++ " is blocked.") gs
    Nothing ->
      wizMsg (showPos p ++ " is outside the map.") gs
  where
    showPos (V2 x y) = "(" ++ show x ++ "," ++ show y ++ ")"

-- | Spawn a monster on the first walkable tile adjacent to the
--   player that isn't already occupied. If the player is somehow
--   boxed in, bail out with a message rather than overwriting
--   something.
wizCmdSpawn :: MonsterKind -> GameState -> GameState
wizCmdSpawn kind gs =
  let neighbors =
        [ gsPlayerPos gs + dirToOffset d | d <- [minBound .. maxBound] ]
      occupied = Set.fromList (map mPos (gsMonsters gs))
      free =
        [ p
        | p <- neighbors
        , case tileAt (gsLevel gs) p of
            Just t  -> isWalkable t
            Nothing -> False
        , not (Set.member p occupied)
        ]
  in case free of
       []      -> wizMsg "no room to spawn next to you." gs
       (p : _) ->
         wizMsg ("spawned a " ++ monsterName kind ++ ".") gs
           { gsMonsters = gsMonsters gs ++ [mkMonster kind p] }

-- | Grant XP and surface the same level-up messages a kill would.
wizCmdXP :: Int -> GameState -> GameState
wizCmdXP n gs
  | n < 0 = wizMsg "XP must be non-negative." gs
  | otherwise =
      let (s', ups) = P.gainXP (gsPlayerStats gs) n
          startLvl  = sLevel (gsPlayerStats gs)
          endLvl    = sLevel s'
          lvlMsgs   =
            [ "You reach level " ++ show l ++ "!"
            | l <- [endLvl, endLvl - 1 .. startLvl + 1]
            ]
      in wizMsg ("granted " ++ show n ++ " XP.") gs
           { gsPlayerStats = s'
           , gsMessages    = lvlMsgs ++ gsMessages gs
           , gsEvents      = gsEvents gs ++ replicate ups EvLevelUp
           }

-- | Force-descend. Unlike 'playerDescend' this does not require
--   standing on a 'StairsDown' tile — it's meant for poking at
--   deeper floors during development.
wizCmdDescend :: GameState -> GameState
wizCmdDescend gs =
  let currentDepth = dlDepth (gsLevel gs)
      nextDepth    = currentDepth + 1
      parked       = parkCurrent gs
      gsParked     = gs { gsLevels = Map.insert currentDepth parked (gsLevels gs) }
      gs' = case Map.lookup nextDepth (gsLevels gsParked) of
        Just pl ->
          loadParked pl
            gsParked { gsLevels = Map.delete nextDepth (gsLevels gsParked) }
        Nothing ->
          generateAndEnter nextDepth gsParked
      gs'' = wizMsg ("descended to depth " ++ show nextDepth ++ ".") gs'
  in fireQuestEvent (EvEnteredDepth nextDepth) gs''

-- | Force-ascend. Refuses at depth 1 with a message, otherwise
--   behaves like 'playerAscend' minus the stairs-tile check.
wizCmdAscend :: GameState -> GameState
wizCmdAscend gs =
  let currentDepth = dlDepth (gsLevel gs)
      prevDepth    = currentDepth - 1
  in if prevDepth < 1
       then wizMsg "already at the top floor." gs
       else
         let parked   = parkCurrent gs
             gsParked = gs { gsLevels = Map.insert currentDepth parked (gsLevels gs) }
         in case Map.lookup prevDepth (gsLevels gsParked) of
              Just pl ->
                let gs' = loadParked pl
                      gsParked { gsLevels = Map.delete prevDepth (gsLevels gsParked) }
                in wizMsg ("ascended to depth " ++ show prevDepth ++ ".") gs'
              Nothing ->
                wizMsg "no parked level to return to." gs

applyAction :: GameAction -> GameState -> GameState
applyAction act gs0 =
  -- Each action starts with a fresh event log so consumers (audio)
  -- only see what happened on *this* turn.
  let gs = gs0 { gsEvents = [] }
  in recomputeVisibility $ case act of
       Quit                -> gs { gsQuitting = True }
       _ | gsDead gs       -> gs
       _ | gsVictory gs    -> gs
       Wait                -> processMonsters gs
       Pickup              -> processMonsters (playerPickup gs)
       UseItem idx         -> processMonsters (playerUseItem idx gs)
       GoDownStairs        -> processMonsters (playerDescend gs)
       GoUpStairs          -> processMonsters (playerAscend gs)
       CloseDoor dir       -> playerCloseDoor dir gs
       Move dir            ->
         let target = gsPlayerPos gs + dirToOffset dir
         in case monsterAt target (gsMonsters gs) of
              Just (i, m) -> processMonsters (playerAttack gs i m)
              Nothing     -> case npcAt target (gsNPCs gs) of
                Just (i, _) ->
                  -- Bump-to-talk: open dialogue, monsters do NOT act.
                  playerTalk i gs
                Nothing     -> case tileAt (gsLevel gs) target of
                  -- Bump-to-open: spending the turn opens a closed
                  -- door in place. The player does not move this
                  -- turn (just like bumping a wall), but the turn
                  -- *does* advance so monsters react.
                  Just (Door Closed) ->
                    let gs' = openDoorAt target gs
                        msg = "You open the door."
                    in processMonsters
                         (gs' { gsMessages = msg : gsMessages gs' })
                  -- Bump-to-unlock: if the player is carrying the
                  -- matching key, consume it, swap the door to
                  -- 'Door Open', advance the turn. Otherwise show a
                  -- "needs the X key" modal and DO NOT advance —
                  -- the failed attempt is a free no-op like bumping
                  -- a wall.
                  Just (Door (Locked kid)) ->
                    case findKeyIndex kid (gsInventory gs) of
                      Just ki ->
                        let inv' = Inv.dropItem ki (gsInventory gs)
                            gs'  = openDoorAt target gs
                            nm   = keyName kid
                            msg  = "You unlock the door with the "
                                ++ nm ++ "."
                        in processMonsters
                             (gs' { gsInventory = inv'
                                  , gsMessages  = msg : gsMessages gs'
                                  })
                      Nothing ->
                        -- no key: raise the modal, no turn cost
                        gs { gsLockedDoorPrompt = Just (keyName kid) }
                  _ ->
                    case M.tryMove (gsLevel gs) (gsPlayerPos gs) dir of
                      Just newPos -> processMonsters (gs { gsPlayerPos = newPos })
                      Nothing     -> gs  -- blocked; turn does not advance

-- | Append events to the running per-turn log.
emit :: GameState -> [GameEvent] -> GameState
emit gs evs = gs { gsEvents = gsEvents gs ++ evs }

-- | Run a 'QuestEvent' through every quest in 'gsQuests' and surface
--   a "Quest complete: NAME!" message for any quest that flips from
--   non-completed to completed as a result. Returns the updated
--   state with the new quest list and any completion messages
--   prepended to 'gsMessages'.
fireQuestEvent :: QuestEvent -> GameState -> GameState
fireQuestEvent ev gs =
  let before      = gsQuests gs
      after       = advanceAll ev before
      -- Pair old and new by position; a quest "just became ready"
      -- if it wasn't ready before and is now. Under M12 goal-met
      -- quests flip to 'QuestReadyToTurnIn' (not 'QuestCompleted')
      -- so this is the right place to tell the player their quest
      -- is waiting on a turn-in.
      newlyReady  =
        [ qName q'
        | (q, q') <- zip before after
        , not (isReady q)
        , isReady q'
        ]
      msgs = [ "Quest ready to turn in: " ++ n ++ "!" | n <- newlyReady ]
  in gs { gsQuests   = after
        , gsMessages = reverse msgs ++ gsMessages gs
        }

-- | Map a combat result to the event the *attacker* cares about
--   when the attacker is the player.
playerCombatEvent :: C.CombatResult -> GameEvent
playerCombatEvent C.Miss            = EvAttackMiss
playerCombatEvent (C.Hit _)         = EvAttackHit
playerCombatEvent (C.CriticalHit _) = EvAttackCrit
playerCombatEvent (C.Kill _)        = EvMonsterKilled

-- | Map a combat result to the event for the player being hit.
--   'Nothing' means "no sound for this" (we skip monster whiffs).
monsterCombatEvent :: C.CombatResult -> Maybe GameEvent
monsterCombatEvent C.Miss            = Nothing
monsterCombatEvent (C.Hit _)         = Just EvPlayerHurt
monsterCombatEvent (C.CriticalHit _) = Just EvPlayerHurt
monsterCombatEvent (C.Kill _)        = Just EvPlayerDied

-- | Find a monster occupying the given tile, if any. Uses
--   'monsterOccupies' so that multi-tile bosses resolve on any
--   tile of their footprint — attacks and collisions that hit any
--   tile of a dragon all point back at the same 'Monster' entry.
monsterAt :: Pos -> [Monster] -> Maybe (Int, Monster)
monsterAt p = go 0
  where
    go _ [] = Nothing
    go i (m : rest)
      | monsterOccupies m p = Just (i, m)
      | otherwise           = go (i + 1) rest

-- | Index of the first 'IKey' in the player's bag whose 'KeyId'
--   matches the given lock, or 'Nothing' if no matching key is
--   present. Used by the bump-to-unlock path to consume the key
--   from the same position it was picked up from.
findKeyIndex :: KeyId -> Inventory -> Maybe Int
findKeyIndex kid inv = go 0 (invItems inv)
  where
    go _ []                       = Nothing
    go i (IKey k : rest) | k == kid = Just i
                         | otherwise = go (i + 1) rest
    go i (_      : rest)            = go (i + 1) rest

-- | Rewrite the tile at 'p' to @Door Open@. Used by the bump-to-open
--   path in 'applyAction' so a closed door becomes walkable on the
--   same turn the player tried to step onto it. Out-of-bounds
--   positions are a no-op (the caller only invokes this after
--   'tileAt' has already confirmed the tile is @Door Closed@).
openDoorAt :: Pos -> GameState -> GameState
openDoorAt (V2 x y) gs =
  let lvl = gsLevel gs
      w   = dlWidth  lvl
      h   = dlHeight lvl
  in if x < 0 || y < 0 || x >= w || y >= h
       then gs
       else let idx      = y * w + x
                newTiles = dlTiles lvl V.// [(idx, Door Open)]
            in gs { gsLevel = lvl { dlTiles = newTiles } }

-- | Rewrite the tile at 'p' to @Door Closed@. Mirrors 'openDoorAt'
--   and is used by 'playerCloseDoor'. The caller is responsible
--   for confirming the tile is currently @Door Open@ and that
--   nothing stands on it.
closeDoorAt :: Pos -> GameState -> GameState
closeDoorAt (V2 x y) gs =
  let lvl = gsLevel gs
      w   = dlWidth  lvl
      h   = dlHeight lvl
  in if x < 0 || y < 0 || x >= w || y >= h
       then gs
       else let idx      = y * w + x
                newTiles = dlTiles lvl V.// [(idx, Door Closed)]
            in gs { gsLevel = lvl { dlTiles = newTiles } }

-- | Attempt to close the door one step in the given direction. On
--   success, stamp 'Door Closed', push a confirmation message, and
--   advance monsters (a successful close costs a turn). On failure
--   (no open door there, or something is standing on the tile),
--   push an explanatory message and DO NOT advance monsters — the
--   failed attempt is a free no-op like bumping a wall.
playerCloseDoor :: Dir -> GameState -> GameState
playerCloseDoor dir gs =
  let target = gsPlayerPos gs + dirToOffset dir
      occupiedByMonster = case monsterAt target (gsMonsters gs) of
        Just _  -> True
        Nothing -> False
      occupiedByNpc = case npcAt target (gsNPCs gs) of
        Just _  -> True
        Nothing -> False
      occupiedByPlayer = target == gsPlayerPos gs
  in case tileAt (gsLevel gs) target of
       Just (Door Open)
         | occupiedByMonster || occupiedByNpc ->
             pushMsg "Something is in the way." gs
         | occupiedByPlayer ->
             -- Can't happen for a Door Open adjacent to the player
             -- unless the player is standing on the door itself,
             -- which only happens if dir is... nothing — but be
             -- defensive anyway.
             pushMsg "You can't close a door you're standing on." gs
         | otherwise ->
             let gs' = closeDoorAt target gs
             in processMonsters (pushMsg "You close the door." gs')
       Just (Door Closed) ->
         pushMsg "That door is already closed." gs
       _ ->
         pushMsg "There is no door there to close." gs
  where
    pushMsg m s = s { gsMessages = m : gsMessages s }

-- | Index lookup mirroring 'monsterAt' but for NPCs.
npcAt :: Pos -> [NPC] -> Maybe (Int, NPC)
npcAt p = go 0
  where
    go _ [] = Nothing
    go i (n : rest)
      | npcPos n == p = Just (i, n)
      | otherwise     = go (i + 1) rest

------------------------------------------------------------
-- NPC dialogue
------------------------------------------------------------

-- | Open the dialogue modal with the NPC at the given index.
--   Does not cost a turn and does not clear the event log (nothing
--   new happened combat-wise).
playerTalk :: Int -> GameState -> GameState
playerTalk i gs = gs { gsDialogue = Just i }

-- | Abandon the currently-active quest at the given index into
--   the *active-only* sub-list of 'gsQuests'. Flipping it to
--   'QuestFailed' is enough — 'advanceQuest' already treats
--   failed as absorbing, so the quest stays visible in the log
--   but never progresses again. Out-of-range indices are no-ops.
--   Clears 'gsQuestLogCursor' after the flip so the log doesn't
--   keep pointing at a now-invalid position.
abandonQuest :: Int -> GameState -> GameState
abandonQuest activeIdx gs =
  let active   = [ (i, q) | (i, q) <- zip [0 ..] (gsQuests gs)
                          , qStatus q == QuestActive ]
  in case drop activeIdx active of
       []              -> gs
       ((realIdx, q) : _) ->
         let failed = q { qStatus = QuestFailed }
             msg    = "You abandon \"" ++ qName q ++ "\"."
         in gs { gsQuests         = updateAt realIdx (const failed) (gsQuests gs)
               , gsQuestLogCursor = Nothing
               , gsMessages       = msg : gsMessages gs
               }

-- | Accept the quest at the given offer index from the NPC at the
--   given NPC index. Moves the quest from the NPC's offer list
--   into 'gsQuests' with its status flipped to 'QuestActive' and
--   its 'qGiver' stamped with the NPC index (so turn-in at that
--   same NPC pays full bounty later). Prepends a confirmation
--   message. If either index is out of range the call is a no-op.
acceptQuestFromNPC :: Int -> Int -> GameState -> GameState
acceptQuestFromNPC npcIdx offerIdx gs =
  case safeIndex npcIdx (gsNPCs gs) of
    Nothing  -> gs
    Just npc -> case safeIndex offerIdx (npcOffers npc) of
      Nothing    -> gs
      Just offer ->
        let accepted  = (acceptOffer offer) { qGiver = Just npcIdx }
            npc'      = npc { npcOffers = removeAt offerIdx (npcOffers npc) }
            npcs'     = updateAt npcIdx (const npc') (gsNPCs gs)
            msg       = "You accept \"" ++ qName accepted ++ "\"."
        in gs { gsNPCs     = npcs'
              , gsQuests   = gsQuests gs ++ [accepted]
              , gsMessages = msg : gsMessages gs
              }
  where
    safeIndex n xs
      | n < 0 || n >= length xs = Nothing
      | otherwise               = Just (xs !! n)

-- | Turn in a ready quest at an NPC. 'questIdx' indexes into the
--   /ready-only/ sub-list of 'gsQuests' (so the dialogue can show
--   only the ready quests without the caller having to remap
--   indices). Preconditions: NPC exists, quest is
--   'QuestReadyToTurnIn'. Full XP bounty when the NPC is the
--   original giver ('qGiver' matches 'npcIdx'), otherwise half
--   (integer division — a reward of 1 at a non-giver pays 0,
--   intentionally: the design discourages tiny quests from being
--   treated identically regardless of giver). Emits
--   'EvQuestTurnedIn' and any 'EvLevelUp's the XP triggers.
turnInQuest :: Int -> Int -> GameState -> GameState
turnInQuest npcIdx readyIdx gs =
  case safeIndex npcIdx (gsNPCs gs) of
    Nothing  -> gs
    Just _ ->
      let ready = [ (i, q) | (i, q) <- zip [0 ..] (gsQuests gs)
                           , qStatus q == QuestReadyToTurnIn ]
      in case drop readyIdx ready of
           []                 -> gs
           ((realIdx, q) : _) ->
             let fullReward = qReward q
                 isOriginal = qGiver q == Just npcIdx
                 awarded    = if isOriginal then fullReward else fullReward `div` 2
                 (s', ups)  = P.gainXP (gsPlayerStats gs) awarded
                 startLvl   = sLevel (gsPlayerStats gs)
                 endLvl     = sLevel s'
                 lvlMsgs    = [ "You reach level " ++ show l ++ "!"
                              | l <- [endLvl, endLvl - 1 .. startLvl + 1] ]
                 completed  = q { qStatus = QuestCompleted }
                 quests'    = updateAt realIdx (const completed) (gsQuests gs)
                 rewardMsg  = if isOriginal
                   then "Quest complete: " ++ qName q ++ "! +" ++ show awarded ++ " XP."
                   else "Quest complete: " ++ qName q ++ "! +" ++ show awarded
                        ++ " XP (partial reward — not the original giver)."
             in gs { gsPlayerStats = s'
                   , gsQuests      = quests'
                   , gsMessages    = lvlMsgs ++ [rewardMsg] ++ gsMessages gs
                   , gsEvents      = gsEvents gs
                                   ++ [EvQuestTurnedIn]
                                   ++ replicate ups EvLevelUp
                   }
  where
    safeIndex n xs
      | n < 0 || n >= length xs = Nothing
      | otherwise               = Just (xs !! n)

playerAttack :: GameState -> Int -> Monster -> GameState
playerAttack gs i m =
  let playerCombat   = Inv.effectiveStats (gsPlayerStats gs) (gsInventory gs)
      (result, gen') = C.resolveAttack (gsRng gs) playerCombat (mStats m)
      newMStats      = C.applyDamage (mStats m) (Damage (C.resultDamage result))
      msg            = C.describeAttack result (monsterName (mKind m))
      killed         = C.isDead newMStats
      wasBoss        = isBoss (mKind m)
      combatEv       = playerCombatEvent result
      (playerStats', levelMsgs, levelEvs) =
        if killed
          then
            let reward     = P.xpReward (mKind m)
                (s', ups)  = P.gainXP (gsPlayerStats gs) reward
                startLevel = sLevel (gsPlayerStats gs)
                endLevel   = sLevel s'
                -- Messages: newest first, so higher levels come first.
                msgs = [ "You reach level " ++ show l ++ "!"
                       | l <- [endLevel, endLevel - 1 .. startLevel + 1]
                       ]
                evs  = replicate ups EvLevelUp
            in (s', msgs, evs)
          else (gsPlayerStats gs, [], [])
      monsters' =
        if killed
          then removeAt i (gsMonsters gs)
          else updateAt i (\mo -> mo { mStats = newMStats }) (gsMonsters gs)
      -- Roll loot drops at the monster's tile if the blow was fatal.
      (loot, gen'') =
        if killed
          then Loot.rollLoot gen' (mKind m)
          else ([], gen')
      lootMsgs =
        [ "The " ++ monsterName (mKind m) ++ " drops a " ++ itemName it ++ "."
        | it <- loot
        ]
      itemsOnFloor' =
        gsItemsOnFloor gs ++ [ (mPos m, it) | it <- loot ]
      -- When the killing blow lands on a boss, append EvBossKilled
      -- and a dedicated victory line, and flip gsVictory so the
      -- render / input layers can show the victory modal and freeze
      -- further input the same way death does.
      bossEvs      = [ EvBossKilled | killed && wasBoss ]
      bossMsgs     = [ "With a final roar, the " ++ monsterName (mKind m) ++ " falls. You are victorious!"
                     | killed && wasBoss ]
      victory'     = gsVictory gs || (killed && wasBoss)
      gs' = emit
        gs
          { gsMonsters     = monsters'
          , gsPlayerStats  = playerStats'
          , gsRng          = gen''
          , gsMessages     = reverse lootMsgs ++ bossMsgs ++ levelMsgs ++ [msg] ++ gsMessages gs
          , gsItemsOnFloor = itemsOnFloor'
          , gsVictory      = victory'
          }
        (combatEv : levelEvs ++ bossEvs)
      -- Fire the generic kill event for every fatal blow, and
      -- additionally fire the boss-specific event so a
      -- 'GoalKillBoss' quest flips to ready. Non-boss kills don't
      -- generate 'EvKilledBoss', so a boss-slaying quest only
      -- advances on the right kill.
      questEvs = if wasBoss then [EvKilledMonster, EvKilledBoss] else [EvKilledMonster]
      fireAll gss = foldl (flip fireQuestEvent) gss questEvs
  in if killed then fireAll gs' else gs'

------------------------------------------------------------
-- Items
------------------------------------------------------------

-- | Pick up the first item on the player's current tile. Costs a
--   turn even if there is nothing to pick up (matching the
--   standard roguelike convention — the attempt still took time).
playerPickup :: GameState -> GameState
playerPickup gs =
  case takeFirstItemAt (gsPlayerPos gs) (gsItemsOnFloor gs) of
    Nothing ->
      gs { gsMessages = "Nothing to pick up." : gsMessages gs }
    Just (item, rest) ->
      case Inv.addItem item (gsInventory gs) of
        Left InventoryFull ->
          gs { gsMessages =
                 ("Your pack is full — you can't pick up the " ++ itemName item ++ ".")
                 : gsMessages gs
             }
        Right inv' ->
          gs { gsInventory    = inv'
             , gsItemsOnFloor = rest
             , gsMessages     = ("You pick up the " ++ itemName item ++ ".") : gsMessages gs
             }

-- | Find and remove the first item at @p@ from the floor list,
--   preserving the order of the rest.
takeFirstItemAt :: Pos -> [(Pos, Item)] -> Maybe (Item, [(Pos, Item)])
takeFirstItemAt p = go []
  where
    go _    []                        = Nothing
    go seen ((q, it) : rest)
      | q == p    = Just (it, reverse seen ++ rest)
      | otherwise = go ((q, it) : seen) rest

-- | Apply the default action to the item at @idx@ in the bag.
--   Potions are quaffed (and consumed); weapons and armor are
--   equipped (swapping with the previously-equipped piece, which
--   goes back to the bag).
playerUseItem :: Int -> GameState -> GameState
playerUseItem idx gs =
  case lookupBag idx (gsInventory gs) of
    Nothing -> gs { gsMessages = "No such item." : gsMessages gs }
    Just item -> case item of
      IPotion p ->
        let inv'    = Inv.dropItem idx (gsInventory gs)
            stats'  = Inv.quaffPotion p (gsPlayerStats gs)
            healed  = sHP stats' - sHP (gsPlayerStats gs)
            msg     = "You quaff the " ++ itemName item
                   ++ " and heal " ++ show healed ++ " HP."
        in gs { gsInventory   = inv'
              , gsPlayerStats = stats'
              , gsMessages    = msg : gsMessages gs
              }
      IWeapon _ ->
        let inv' = Inv.equip idx (gsInventory gs)
            msg  = "You equip the " ++ itemName item ++ "."
        in gs { gsInventory = inv', gsMessages = msg : gsMessages gs }
      IArmor _ ->
        let inv' = Inv.equip idx (gsInventory gs)
            msg  = "You don the " ++ itemName item ++ "."
        in gs { gsInventory = inv', gsMessages = msg : gsMessages gs }
      IKey _ ->
        -- Keys aren't "used" from the inventory screen; the player
        -- bumps the matching locked door and the key is consumed
        -- automatically. Explain why nothing happened.
        let msg = "You fiddle with the " ++ itemName item
               ++ ". It probably fits a door somewhere."
        in gs { gsMessages = msg : gsMessages gs }
  where
    lookupBag i inv
      | i < 0 || i >= length (invItems inv) = Nothing
      | otherwise                           = Just (invItems inv !! i)

------------------------------------------------------------
-- Stairs and level transitions
------------------------------------------------------------

-- | Park the current level state so it can be restored later.
parkCurrent :: GameState -> ParkedLevel
parkCurrent gs = ParkedLevel
  { plLevel     = gsLevel gs
  , plMonsters  = gsMonsters gs
  , plItems     = gsItemsOnFloor gs
  , plExplored  = gsExplored gs
  , plPlayerPos = gsPlayerPos gs
  , plBossRoom  = gsBossRoom gs
  }

-- | Swap a 'ParkedLevel' in as the current level. The caller is
--   responsible for removing it from 'gsLevels' if appropriate
--   (so we don't leave a stale parked copy lying around).
loadParked :: ParkedLevel -> GameState -> GameState
loadParked pl gs = gs
  { gsLevel        = plLevel pl
  , gsMonsters     = plMonsters pl
  , gsItemsOnFloor = plItems pl
  , gsExplored     = plExplored pl
  , gsPlayerPos    = plPlayerPos pl
  , gsBossRoom     = plBossRoom pl
  -- Any AI-generated flavor text from the floor we're leaving is
  -- no longer relevant — drop it so the panel doesn't linger into
  -- the new (old) floor. The process-local dedup set in the AI
  -- runtime still remembers which rooms have been described, so
  -- the old text is simply not re-fetched.
  , gsRoomDesc        = Nothing
  , gsRoomDescVisible = False
  }

-- | Freshly generate the next floor and swap it in. The new level
--   inherits the 'LevelConfig' from 'defaultLevelConfig' except
--   for its depth, which is set to the supplied value. The player
--   lands on the new level's 'StairsUp' tile (that's where the
--   generator places @startPos@).
--
--   If @depth@ matches the run's rolled 'gsBossDepth', the floor is
--   post-processed into a boss floor: 'StairsDown' is stripped (the
--   dragon is literally the end of the line), the last room is
--   designated the boss room, the dragon is spawned at a random
--   interior position with 2x2 footprint clearance, and regular
--   monsters are spawned in every room *except* the boss room so
--   the boss fight is clean.
generateAndEnter :: Int -> GameState -> GameState
generateAndEnter depth gs =
  let cfg            = D.defaultLevelConfig { D.lcDepth = depth }
      (dl0, start, rooms, mLocked, nextKey1, g1) =
        D.generateLevel (gsRng gs) cfg (gsNextKeyId gs)
      isBossFloor   = depth == gsBossDepth gs && not (null rooms)
      -- If this level minted a lock, the key is placed on this
      -- same floor (not deferred) and must land on the spawn-side
      -- of the lock so the player can reach it without opening the
      -- door. 'spawnSideReachable' walks the level treating locked
      -- doors as walls, giving us the keyless-reachable tile set.
      reachable     = spawnSideReachable dl0 start
      newKeys       = case mLocked of
        Just (kid, _) -> [kid]
        Nothing       -> []
      -- Any keys left over from earlier floors' scheduling (should
      -- always be empty now that 'scheduleKeyDrop' is same-floor,
      -- but we still honor the list for save-compat) get drained
      -- when their target depth matches.
      (drainedPending, remainingPending) =
        ( [ kid | (d, kid) <- gsPendingKeys gs, d == depth ]
        , [ e   | e@(d, _) <- gsPendingKeys gs, d /= depth ]
        )
      keysToPlace   = newKeys ++ drainedPending
      (keyLoot, g2) = placeKeyLoot g1 reachable rooms keysToPlace
  in if isBossFloor
       then
         let bossRoom            = last rooms
             -- Rooms that still get regular spawns: everything except
             -- the starting room (index 0) and the boss room.
             regularRooms        = drop 1 (init rooms)
             (regulars, g3)      = spawnMonsters g2 regularRooms
             (dragonPos, g4)     = pickBossTopLeft bossRoom g3
             dragon              = mkMonster Dragon dragonPos
             dl                  = D.stripStairsDown dl0
         in gs
              { gsLevel        = dl
              , gsMonsters     = dragon : regulars
              , gsItemsOnFloor = keyLoot
              , gsExplored     = Set.empty
              , gsPlayerPos    = start
              , gsRng          = g4
              , gsBossRoom     = Just bossRoom
              , gsRoomDesc        = Nothing
              , gsRoomDescVisible = False
              , gsNextKeyId    = nextKey1
              , gsPendingKeys  = remainingPending
              }
       else
         let spawnRooms     = drop 1 rooms
             (monsters, g3) = spawnMonsters g2 spawnRooms
         in gs
              { gsLevel        = dl0
              , gsMonsters     = monsters
              , gsItemsOnFloor = keyLoot
              , gsExplored     = Set.empty
              , gsPlayerPos    = start
              , gsRng          = g3
              , gsBossRoom     = Nothing
              , gsRoomDesc        = Nothing
              , gsRoomDescVisible = False
              , gsNextKeyId    = nextKey1
              , gsPendingKeys  = remainingPending
              }

-- | Drop every pending key onto a random floor tile inside a room
--   that the player can reach from spawn /without/ opening any
--   locked door — i.e. a room whose tiles intersect the
--   'spawnSideReachable' set for this level. Prefers non-spawn
--   rooms (so the player doesn't trip over the key on the same step
--   they enter the floor) and falls back to the spawn room if no
--   non-spawn room is on the spawn side. Each key becomes one
--   @(Pos, IKey kid)@ entry suitable for 'gsItemsOnFloor'.
--
--   The reachability filter is what prevents a softlock where the
--   lock sits between spawn and the key itself.
placeKeyLoot
  :: StdGen
  -> Set Pos
  -> [D.Room]
  -> [KeyId]
  -> ([(Pos, Item)], StdGen)
placeKeyLoot gen0 reachable rooms keys =
  let -- Tiles of a room that are actually reachable (pre-filtered).
      roomReachableTiles r =
        [ p | p <- roomPositions r, Set.member p reachable ]

      -- Rooms that still have at least one reachable tile.
      reachableRooms = filter (not . null . roomReachableTiles) rooms

      -- Prefer non-spawn rooms (index >= 1). If none of those are
      -- reachable (pathological: the locked door seals off
      -- everything past the spawn room), fall back to the spawn
      -- room alone. If nothing is reachable at all, the key is
      -- silently dropped — at that point the level is broken in
      -- ways unrelated to locked doors.
      preferredRooms =
        let nonSpawn = filter (not . null . roomReachableTiles) (drop 1 rooms)
        in if null nonSpawn
             then case rooms of
                    (r : _) | not (null (roomReachableTiles r)) -> [r]
                    _                                            -> []
             else nonSpawn

      pool
        | not (null preferredRooms) = preferredRooms
        | otherwise                 = reachableRooms

      go g [] = ([], g)
      go g (k : ks) = case pool of
        [] -> go g ks  -- nothing reachable: drop the key
        _  ->
          let (ri, g1)    = randomR (0, length pool - 1) g
              room        = pool !! ri
              tiles       = roomReachableTiles room
              (ti, g2)    = randomR (0, length tiles - 1) g1
              pos         = tiles !! ti
              (rest, g3)  = go g2 ks
          in ((pos, IKey k) : rest, g3)
  in go gen0 keys

-- | Every @(x, y)@ floor tile inside a room's rectangle. Rooms are
--   carved as solid floor, so every position here is guaranteed to
--   be a walkable tile in the generated level.
roomPositions :: D.Room -> [Pos]
roomPositions r =
  [ V2 x y
  | x <- [D.rX r .. D.rX r + D.rW r - 1]
  , y <- [D.rY r .. D.rY r + D.rH r - 1]
  ]

-- | Should the boss music track be playing right now? True iff the
--   player is currently standing on the boss floor /and/ has
--   explored at least one tile of the boss room — i.e. they've
--   laid eyes on the dragon's lair at some point. Stays true even
--   if the player retreats out of line of sight (the music would
--   otherwise flicker every few steps), and flips back to false
--   when they climb away from the boss floor entirely.
shouldPlayBossMusic :: GameState -> Bool
shouldPlayBossMusic gs = case gsBossRoom gs of
  Nothing   -> False
  Just room ->
    let tiles =
          [ V2 x y
          | x <- [D.rX room .. D.rX room + D.rW room - 1]
          , y <- [D.rY room .. D.rY room + D.rH room - 1]
          ]
    in any (`Set.member` gsExplored gs) tiles

-- | Pick a random top-left position inside a room such that a 2x2
--   footprint fits entirely within the room's interior. For a room
--   with width or height of exactly 1 (shouldn't happen given
--   'lcRoomMin' = 4) this degenerates to the top-left corner.
pickBossTopLeft :: D.Room -> StdGen -> (Pos, StdGen)
pickBossTopLeft r gen0 =
  let xMax    = D.rX r + max 0 (D.rW r - 2)
      yMax    = D.rY r + max 0 (D.rH r - 2)
      (x, g1) = randomR (D.rX r, xMax) gen0
      (y, g2) = randomR (D.rY r, yMax) g1
  in (V2 x y, g2)

-- | Descend one floor. Fails with a flavor message if the player
--   is not standing on 'StairsDown'. If the next floor has been
--   visited before, it is restored from 'gsLevels'; otherwise it
--   is generated fresh.
playerDescend :: GameState -> GameState
playerDescend gs =
  case tileAt (gsLevel gs) (gsPlayerPos gs) of
    Just StairsDown ->
      let currentDepth = dlDepth (gsLevel gs)
          nextDepth    = currentDepth + 1
          parked       = parkCurrent gs
          gsParked     = gs { gsLevels = Map.insert currentDepth parked (gsLevels gs) }
          gs' = case Map.lookup nextDepth (gsLevels gsParked) of
            Just pl ->
              loadParked pl
                gsParked { gsLevels = Map.delete nextDepth (gsLevels gsParked) }
            Nothing ->
              generateAndEnter nextDepth gsParked
          gs'' = gs' { gsMessages = ("You descend to depth " ++ show nextDepth ++ ".") : gsMessages gs' }
      in fireQuestEvent (EvEnteredDepth nextDepth) gs''
    _ ->
      gs { gsMessages = "There are no stairs down here." : gsMessages gs }

-- | Ascend one floor. Fails if the player is not on 'StairsUp' or
--   if there is nowhere to go (i.e. we're already at depth 1).
playerAscend :: GameState -> GameState
playerAscend gs =
  case tileAt (gsLevel gs) (gsPlayerPos gs) of
    Just StairsUp ->
      let currentDepth = dlDepth (gsLevel gs)
          prevDepth    = currentDepth - 1
      in if prevDepth < 1
           then gs { gsMessages = "These stairs lead to daylight — there's nowhere further up." : gsMessages gs }
           else
             let parked  = parkCurrent gs
                 gsParked = gs { gsLevels = Map.insert currentDepth parked (gsLevels gs) }
             in case Map.lookup prevDepth (gsLevels gsParked) of
                  Just pl ->
                    let gs' = loadParked pl
                          gsParked { gsLevels = Map.delete prevDepth (gsLevels gsParked) }
                    in gs' { gsMessages = ("You climb to depth " ++ show prevDepth ++ ".") : gsMessages gs' }
                  Nothing ->
                    -- Shouldn't happen: you can only go up if you came from there.
                    gs { gsMessages = "The way up is blocked by your own memory of having been there." : gsMessages gs }
    _ ->
      gs { gsMessages = "There are no stairs up here." : gsMessages gs }

------------------------------------------------------------
-- Monster turns
------------------------------------------------------------

processMonsters :: GameState -> GameState
processMonsters gs0 = go gs0 0
  where
    go gs i
      | gsDead gs                    = gs
      | i >= length (gsMonsters gs)  = gs
      | otherwise                    =
          let m = gsMonsters gs !! i
          in if C.isDead (mStats m)
               then go gs (i + 1)
               else go (processMonster gs i m) (i + 1)

processMonster :: GameState -> Int -> Monster -> GameState
processMonster gs i m =
  let dl        = gsLevel gs
      playerPos = gsPlayerPos gs
      -- Every tile occupied by every /other/ monster — multi-tile
      -- bosses contribute all of their footprint tiles here, so
      -- a dragon next to a rat blocks every tile of its own
      -- footprint, not just its top-left.
      others    = concat
        [ monsterTiles x
        | (j, x) <- zip [0 :: Int ..] (gsMonsters gs)
        , j /= i
        ]
      intent    = monsterIntent dl playerPos others monsterSightRadius m
  in case intent of
       MiWait -> gs
       MiMove newPos ->
         gs { gsMonsters = updateAt i (\mo -> mo { mPos = newPos }) (gsMonsters gs) }
       MiAttack -> monsterAttack gs m

monsterAttack :: GameState -> Monster -> GameState
monsterAttack gs m =
  let playerDefense   = Inv.effectiveStats (gsPlayerStats gs) (gsInventory gs)
      (result, gen')  = C.resolveAttack (gsRng gs) (mStats m) playerDefense
      newPlayerStats  = C.applyDamage (gsPlayerStats gs) (Damage (C.resultDamage result))
      msg             = C.describeAttacked result (monsterName (mKind m))
      died            = C.isDead newPlayerStats
      newMsgs         = if died then ["You die...", msg] else [msg]
      evs             = case monsterCombatEvent result of
        Just e  -> [e]
        Nothing -> []
  in emit
       gs
         { gsPlayerStats = newPlayerStats
         , gsRng         = gen'
         , gsMessages    = newMsgs ++ gsMessages gs
         , gsDead        = died
         }
       evs

------------------------------------------------------------
-- List helpers
------------------------------------------------------------

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f xs = take i xs ++ [f (xs !! i)] ++ drop (i + 1) xs

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i + 1) xs
