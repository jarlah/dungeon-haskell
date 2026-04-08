module Game.GameState
  ( GameState(..)
  , ParkedLevel(..)
  , NPC(..)
  , mkGameState
  , newGame
  , defaultPlayerStats
  , hardcodedRoom
  , hardcodedInitialState
  , applyAction
  , applyCommand
  , acceptQuestFromNPC
  , abandonQuest
  , fovRadius
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Vector as V
import Linear (V2(..))
import System.Random (StdGen, mkStdGen, randomR)

import Game.Types
import qualified Game.Logic.Combat as C
import Game.Logic.Combat (Damage(..))
import Game.Logic.Command (Command(..))
import qualified Game.Logic.Dungeon as D
import qualified Game.Logic.FOV as FOV
import qualified Game.Logic.Inventory as Inv
import qualified Game.Logic.Loot as Loot
import Game.Logic.MonsterAI (MonsterIntent(..), monsterIntent)
import qualified Game.Logic.Movement as M
import qualified Game.Logic.Progression as P
import Game.Logic.Quest
  ( Quest(..), QuestEvent(..), QuestGoal(..), QuestStatus(..)
  , advanceAll, isCompleted, mkQuest
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
  , gsLevels      :: !(Map Int ParkedLevel)
    -- ^ previously-visited dungeon levels, keyed by their depth.
    --   The *current* level is always in 'gsLevel' and friends —
    --   parked entries only exist for floors the player has left
    --   but may return to. Each parked level remembers its
    --   monsters, items, explored set, and the position the player
    --   was standing on when they left, so going back up returns
    --   them to exactly where they descended from.
  } deriving (Show)

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
    -- ^ one-line flavor text shown at the top of the dialogue
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
  , gsLevels      = Map.empty
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

-- | Create a fresh game: generate a level, spawn monsters, build state.
newGame :: StdGen -> D.LevelConfig -> GameState
newGame gen0 cfg =
  let (dl, startPos, rooms, gen1) = D.generateLevel gen0 cfg
      -- Don't spawn monsters in the player's starting room.
      spawnRooms       = drop 1 rooms
      (monsters, gen2) = spawnMonsters gen1 spawnRooms
      (npcs,     gen3) = spawnNPCs gen2 (D.lcDepth cfg) rooms
  in (mkGameState gen3 dl startPos monsters) { gsNPCs = npcs }

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
              , npcOffers   =
                  [ mkOffer "Slayer" (GoalKillMonsters 5)
                  , mkOffer "Delve"  (GoalReachDepth 3)
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
          m = Monster kind p (monsterStats kind)
          (rest, g3) = spawnInRoom g2 r (n - 1)
      in (m : rest, g3)

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

-- | Apply a parsed slash-command. Commands are wizard / debug
--   helpers right now — they do not cost a turn, do not emit game
--   events, and do not advance monsters. Adding a gameplay command
--   later (e.g. @/pray@) would route through 'applyAction' instead.
--
--   Each command wraps its effect in 'recomputeVisibility' at the
--   end so anything that moves the player or reshapes the level
--   leaves 'gsVisible' / 'gsExplored' consistent.
applyCommand :: Command -> GameState -> GameState
applyCommand cmd gs = recomputeVisibility $ case cmd of
  CmdReveal       -> wizCmdReveal gs
  CmdHeal         -> wizCmdHeal gs
  CmdKillAll      -> wizCmdKillAll gs
  CmdTeleport p   -> wizCmdTeleport p gs
  CmdSpawn k      -> wizCmdSpawn k gs
  CmdXP n         -> wizCmdXP n gs
  CmdDescend      -> wizCmdDescend gs
  CmdAscend       -> wizCmdAscend gs

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
         let m = Monster
               { mKind  = kind
               , mPos   = p
               , mStats = monsterStats kind
               }
         in wizMsg ("spawned a " ++ monsterName kind ++ ".") gs
              { gsMonsters = gsMonsters gs ++ [m] }

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
       Wait                -> processMonsters gs
       Pickup              -> processMonsters (playerPickup gs)
       UseItem idx         -> processMonsters (playerUseItem idx gs)
       GoDownStairs        -> processMonsters (playerDescend gs)
       GoUpStairs          -> processMonsters (playerAscend gs)
       Move dir            ->
         let target = gsPlayerPos gs + dirToOffset dir
         in case monsterAt target (gsMonsters gs) of
              Just (i, m) -> processMonsters (playerAttack gs i m)
              Nothing     -> case npcAt target (gsNPCs gs) of
                Just (i, _) ->
                  -- Bump-to-talk: open dialogue, monsters do NOT act.
                  playerTalk i gs
                Nothing     ->
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
      -- Pair old and new by position; a quest "just completed" if
      -- it wasn't completed before and is now.
      newlyDone   =
        [ qName q'
        | (q, q') <- zip before after
        , not (isCompleted q)
        , isCompleted q'
        ]
      msgs = [ "Quest complete: " ++ n ++ "!" | n <- newlyDone ]
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

monsterAt :: Pos -> [Monster] -> Maybe (Int, Monster)
monsterAt p = go 0
  where
    go _ [] = Nothing
    go i (m : rest)
      | mPos m == p = Just (i, m)
      | otherwise   = go (i + 1) rest

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
--   into 'gsQuests' with its status flipped to 'QuestActive', and
--   prepends a confirmation message. If either index is out of
--   range the call is a no-op (defensive; Main shouldn't produce
--   bad indices but we guard against it).
acceptQuestFromNPC :: Int -> Int -> GameState -> GameState
acceptQuestFromNPC npcIdx offerIdx gs =
  case safeIndex npcIdx (gsNPCs gs) of
    Nothing  -> gs
    Just npc -> case safeIndex offerIdx (npcOffers npc) of
      Nothing    -> gs
      Just offer ->
        let accepted  = acceptOffer offer
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

playerAttack :: GameState -> Int -> Monster -> GameState
playerAttack gs i m =
  let playerCombat   = Inv.effectiveStats (gsPlayerStats gs) (gsInventory gs)
      (result, gen') = C.resolveAttack (gsRng gs) playerCombat (mStats m)
      newMStats      = C.applyDamage (mStats m) (Damage (C.resultDamage result))
      msg            = C.describeAttack result (monsterName (mKind m))
      killed         = C.isDead newMStats
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
      gs' = emit
        gs
          { gsMonsters     = monsters'
          , gsPlayerStats  = playerStats'
          , gsRng          = gen''
          , gsMessages     = reverse lootMsgs ++ levelMsgs ++ [msg] ++ gsMessages gs
          , gsItemsOnFloor = itemsOnFloor'
          }
        (combatEv : levelEvs)
  in if killed then fireQuestEvent EvKilledMonster gs' else gs'

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
  }

-- | Freshly generate the next floor and swap it in. The new level
--   inherits the 'LevelConfig' from 'defaultLevelConfig' except
--   for its depth, which is set to the supplied value. The player
--   lands on the new level's 'StairsUp' tile (that's where the
--   generator places @startPos@).
generateAndEnter :: Int -> GameState -> GameState
generateAndEnter depth gs =
  let cfg            = D.defaultLevelConfig { D.lcDepth = depth }
      (dl, start, rooms, g1) = D.generateLevel (gsRng gs) cfg
      spawnRooms     = drop 1 rooms
      (monsters, g2) = spawnMonsters g1 spawnRooms
  in gs
       { gsLevel        = dl
       , gsMonsters     = monsters
       , gsItemsOnFloor = []
       , gsExplored     = Set.empty
       , gsPlayerPos    = start
       , gsRng          = g2
       }

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
      others    = [ mPos x | (j, x) <- zip [0 :: Int ..] (gsMonsters gs), j /= i ]
      intent    = monsterIntent dl playerPos others (mPos m)
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
