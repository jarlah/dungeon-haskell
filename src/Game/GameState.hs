module Game.GameState
  ( GameState(..)
  , ParkedLevel(..)
  , NPC(..)
  , LaunchOption(..)
  , LaunchMenu(..)
  , SaveMenuMode(..)
  , SaveMenuEntry(..)
  , DirectionalAction(..)
  , SaveMenu(..)
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
  , regenInterval
  , tickPlayerTurn
  , tickRegen
  , tickTurnCounter
  , runRank
  , playerUseItem
  , playerAttack
  , fireArrow
  , playerOpenChest
  , chestAt
  , monsterAt
  , npcAt
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Vector as V
import Linear (V2(..))
import System.Random (StdGen, mkStdGen, randomR)

import Game.Types
import Game.Logic.Chest (Chest(..), ChestState(..), chestRespawnTurns)
import Game.Logic.Combat (Damage(..))
import Game.Logic.Command (Command(..), isCheatCommand)
import qualified Game.Logic.Chest as Chest
import qualified Game.Logic.Combat as C
import qualified Game.Logic.Dungeon as D
import qualified Game.Logic.FOV as FOV
import qualified Game.Logic.Inventory as Inv
import qualified Game.Logic.Ranged as Ranged
import qualified Game.Logic.Movement as M
import qualified Game.Logic.Progression as P
import Game.Logic.MonsterAI (MonsterIntent(..), monsterIntent)
import Game.Logic.Quest
  ( Quest(..), QuestEvent(..), QuestGoal(..), QuestStatus(..)
  , mkQuest, fireQuestEvent
  )
import Data.Maybe (isJust)
import Game.State.Types
  ( LaunchOption(..), GameState(..), NPC(..), ParkedLevel(..)
  , LaunchMenu(..), SaveMenu(..), SaveMenuMode(..), SaveMenuEntry(..)
  , DirectionalAction(..), emit
  )
import Game.Utils.List (updateAt, removeAt)

-- | The fixed order of options on the launch screen. Kept as a
--   top-level list so the renderer and the key handler agree on
--   indices without having to duplicate the list.
launchOptions :: [LaunchOption]
launchOptions = [LaunchNewGame, LaunchContinue, LaunchLoad, LaunchQuit]

-- | How far a dash moves the player, in tiles, before stopping on
--   any obstacle (wall, closed/locked door, monster, NPC, item, or
--   stairs). Five is enough to break line of sight against most
--   regular monsters' 8-tile sight radius after two dashes, which
--   is the scenario the mechanic exists to support.
dashMaxSteps :: Int
dashMaxSteps = 5

-- | How many turns must pass after a dash before the player can
--   dash again. Ticks in 'processMonsters', which runs once per
--   turn-advancing action.
dashCooldownTurns :: Int
dashCooldownTurns = 60

-- | How many consecutive "safe" turns (no hostile monster visible
--   in the player's FOV) the player must accumulate before
--   regenerating 1 HP via 'tickRegen'. Twelve feels slow enough
--   that potions still matter in active combat but fast enough
--   that retreating behind a closed door actually pays off.
regenInterval :: Int
regenInterval = 12

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
  { sHP      = 25
  , sMaxHP   = 25
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
  , gsChests       = []
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
  , gsDashCooldown      = 0
  , gsRegenCounter      = 0
  , gsTurnsElapsed      = 0
  , gsPotionsUsed       = 0
  , gsSavesUsed         = 0
  , gsFinalTurns        = Nothing
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
      (monsters, gen3) = spawnMonsters gen2 (D.lcDepth cfg) spawnRooms
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
      -- Depth 1 always seeds exactly one chest in a non-starting
      -- room, so the tutorial floor introduces the mechanic. Depths
      -- 2-3 are deliberately chest-less (see 'generateAndEnter'),
      -- and deeper floors get their chests rolled when they're
      -- first generated.
      occupiedForChests =
          startPos
        : concatMap monsterTiles monsters
        ++ map npcPos npcs
        ++ map fst keyLoot
      (chests, gen6) =
        placeChests gen5 dl (drop 1 rooms) occupiedForChests 1
      base             = mkGameState gen6 dl startPos monsters
  in base
       { gsNPCs         = npcs
       , gsBossDepth    = bossDepth
       , gsNextKeyId    = nextKey
       , gsPendingKeys  = []
       , gsItemsOnFloor = keyLoot
       , gsChests       = chests
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

-- | Roll 0-2 monsters per candidate room and drop them in random
--   spots. The floor depth gates which monster kinds may appear:
--   depth 1 is a tutorial floor with only rats and goblins, so a
--   fresh player can find a sword and level up before meeting an
--   orc (see 'randomMonsterKind').
spawnMonsters :: StdGen -> Int -> [D.Room] -> ([Monster], StdGen)
spawnMonsters gen0 depth = foldl' step ([], gen0)
  where
    step (acc, gen) r =
      let (count, g1) = randomR (0 :: Int, 2) gen
          (ms,    g2) = spawnInRoom g1 depth r count
      in (acc ++ ms, g2)

spawnInRoom :: StdGen -> Int -> D.Room -> Int -> ([Monster], StdGen)
spawnInRoom gen0 depth r n
  | n <= 0    = ([], gen0)
  | otherwise =
      let (kind, g1) = randomMonsterKind depth gen0
          (p,    g2) = randomRoomPos r g1
          m          = mkMonster kind p
          (rest, g3) = spawnInRoom g2 depth r (n - 1)
      in (m : rest, g3)

-- | Pick a random non-boss monster kind. Depth 1 only rolls rats
--   and goblins so new players have a tutorial floor to find a
--   weapon and level up; depth 2+ opens the full roster (currently
--   rat/goblin/orc, uniform). Bosses are placed by 'spawnBoss', not
--   by this roll.
randomMonsterKind :: Int -> StdGen -> (MonsterKind, StdGen)
randomMonsterKind depth gen0
  | depth <= 1 =
      let (i, gen1) = randomR (0 :: Int, 1) gen0
          k = case i of
                0 -> Rat
                _ -> Goblin
      in (k, gen1)
  | otherwise =
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
      (quests', qMsgs) = fireQuestEvent (EvEnteredDepth nextDepth) (gsQuests gs'')
    in gs'' { gsQuests = quests', gsMessages = reverse qMsgs ++ gsMessages gs'' }

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
       Dash dir            -> playerDash dir gs
       Fire dir            ->
         let gs' = fireArrow dir gs
             -- Fire only costs a turn when an arrow actually
             -- left the quiver. If the arrow count is unchanged,
             -- the attempt failed (no bow / no arrows) and the
             -- turn is a free no-op, matching dash-on-cooldown.
             shot = invArrows (gsInventory gs')
                  < invArrows (gsInventory gs)
         in if shot then processMonsters gs' else gs'
       Move dir            ->
         let target = gsPlayerPos gs + dirToOffset dir
         in case monsterAt target (gsMonsters gs) of
              Just (i, m) -> processMonsters (playerAttack gs i m)
              Nothing     -> case npcAt target (gsNPCs gs) of
                Just (i, _) ->
                  -- Bump-to-talk: open dialogue, monsters do NOT act.
                  playerTalk i gs
                Nothing -> case chestAt target (gsChests gs) of
                  -- Bump-to-open: stepping into a chest spends the
                  -- turn opening it (or acknowledging it's empty),
                  -- same rhythm as bumping a closed door. The player
                  -- does NOT move onto the chest's tile.
                  Just (ci, c) ->
                    processMonsters (playerOpenChest ci c gs)
                  Nothing -> case tileAt (gsLevel gs) target of
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
                    -- 'Door Open', advance the turn. Otherwise show
                    -- a "needs the X key" modal and DO NOT advance —
                    -- the failed attempt is a free no-op like
                    -- bumping a wall.
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

-- | Escape dash. Moves the player up to 'dashMaxSteps' tiles in the
--   given direction, stopping one tile before the first obstacle.
--   An obstacle is anything that isn't 'Floor' or @Door Open@ (so
--   walls, closed and locked doors, and stairs all stop the dash)
--   or any tile occupied by a monster, NPC, or floor item. This
--   keeps the dash a pure movement tool — it will never combat,
--   open doors, descend, or pick up items behind the player's back.
--
--   Requires @gsDashCooldown == 0@ and at least one successful step
--   in the given direction. On success, the player's position is
--   updated, a message is logged, the cooldown is set to
--   'dashCooldownTurns', and monsters take a turn. On failure (on
--   cooldown, or blocked at the first step) the state is returned
--   with a failure message and /no turn is spent/ — exactly like
--   bumping a wall.
playerDash :: Dir -> GameState -> GameState
playerDash dir gs
  | gsDashCooldown gs > 0 =
      gs { gsMessages =
             ("Dash not ready (" ++ show (gsDashCooldown gs)
               ++ " turns).")
             : gsMessages gs
         }
  | otherwise =
      let steps  = dashSteps gs dir dashMaxSteps
          nTaken = length steps
      in if nTaken == 0
           then gs { gsMessages = "You can't dash that way." : gsMessages gs }
           else
             let newPos = last steps
                 gs'    = gs
                   { gsPlayerPos    = newPos
                   , gsDashCooldown = dashCooldownTurns
                   , gsMessages     =
                       ("You dash " ++ show nTaken ++ " steps.")
                       : gsMessages gs
                   }
             in processMonsters gs'

-- | Accumulate up to @n@ successive positions in direction @dir@
--   starting from the player's current position. Stops as soon as
--   the next step would land on anything other than plain floor or
--   an open door, anything occupied by a monster / NPC, or anything
--   with a floor item on it. Returns the list of /positions stepped
--   onto/ (empty list = blocked at step 1).
dashSteps :: GameState -> Dir -> Int -> [Pos]
dashSteps gs dir n = go n (gsPlayerPos gs)
  where
    dl       = gsLevel gs
    monsters = gsMonsters gs
    npcs     = gsNPCs gs
    items    = gsItemsOnFloor gs
    offset   = dirToOffset dir

    go 0 _ = []
    go k p =
      let next = p + offset
      in if not (dashPassable next)
           then []
           else next : go (k - 1) next

    dashPassable p =
      case tileAt dl p of
        Just Floor       -> clearOfActors p
        Just (Door Open) -> clearOfActors p
        _                -> False

    clearOfActors p =
         case monsterAt p monsters of { Just _ -> False; Nothing -> True }
      && case npcAt     p npcs      of { Just _ -> False; Nothing -> True }
      && not (any ((== p) . fst) items)

-- | Index lookup mirroring 'monsterAt' but for NPCs.
npcAt :: Pos -> [NPC] -> Maybe (Int, NPC)
npcAt p = go 0
  where
    go _ [] = Nothing
    go i (n : rest)
      | npcPos n == p = Just (i, n)
      | otherwise     = go (i + 1) rest

-- | Index lookup mirroring 'monsterAt' but for chests. Returns the
--   first chest whose 'chestPos' matches, along with its position
--   in 'gsChests'. Used by the bump-to-open path in 'applyAction'.
chestAt :: Pos -> [Chest] -> Maybe (Int, Chest)
chestAt p = go 0
  where
    go _ [] = Nothing
    go i (c : rest)
      | chestPos c == p = Just (i, c)
      | otherwise       = go (i + 1) rest

-- | Replace the chest at index @i@ with @c'@, leaving the rest of
--   the list untouched. Total over in-range indices; out-of-range
--   indices pass the list through unchanged (defensive — callers
--   use 'chestAt' to find the index in the first place).
replaceChestAt :: Int -> Chest -> [Chest] -> [Chest]
replaceChestAt i c' = go 0
  where
    go _ []       = []
    go j (c : cs)
      | j == i    = c' : cs
      | otherwise = c  : go (j + 1) cs

-- | Bump-to-open chest interaction. Mirrors 'playerPickup'/the
--   bump-to-open door flow:
--
--     * 'ChestFull' → try to stuff the item into the bag. If the
--       bag is full, drop the item on the floor at the chest's
--       tile instead (so nothing is ever lost). Either way the
--       chest flips to 'ChestEmpty chestRespawnTurns'.
--     * 'ChestEmpty' → push a "The chest is empty." message. The
--       turn still advances (the caller wraps this in
--       'processMonsters') — bumping a chest is an action just
--       like bumping a door.
playerOpenChest :: Int -> Chest -> GameState -> GameState
playerOpenChest i c gs = case chestState c of
  ChestEmpty _ ->
    gs { gsMessages = "The chest is empty." : gsMessages gs }
  ChestFull item ->
    let emptied = c { chestState = ChestEmpty chestRespawnTurns }
        chests' = replaceChestAt i emptied (gsChests gs)
    in case item of
         -- Arrow bundles stack into 'invArrows' instead of
         -- taking an inventory slot, so they can't fail on a
         -- full bag and never land on the floor from a chest.
         IArrows n ->
           let inv  = gsInventory gs
               inv' = inv { invArrows = invArrows inv + n }
           in gs { gsInventory = inv'
                 , gsChests    = chests'
                 , gsMessages  =
                     ("You open the chest and find a "
                      ++ itemName item ++ ".")
                     : gsMessages gs
                 }
         _ -> case Inv.addItem item (gsInventory gs) of
           Right inv' ->
             gs { gsInventory = inv'
                , gsChests    = chests'
                , gsMessages  =
                    ("You open the chest and find a "
                     ++ itemName item ++ ".")
                    : gsMessages gs
                }
           Left InventoryFull ->
             -- Bag is full: drop the item on the chest's tile so
             -- the player can pick it up after freeing a slot.
             -- Mirrors the "full bag" fallback the plan calls for.
             gs { gsChests       = chests'
                , gsItemsOnFloor = (chestPos c, item) : gsItemsOnFloor gs
                , gsMessages     =
                    ("The chest holds a " ++ itemName item
                     ++ ", but your pack is full — it spills onto the floor.")
                    : gsMessages gs
                }

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
      msg            = C.describeAttack result (monsterName (mKind m))
  in C.applyHitResult gs { gsRng = gen' } i m result [msg]

-- | Ranged attack: fire one arrow in the given direction. The
--   caller in 'applyAction' uses the arrow-count delta to decide
--   whether the turn actually advanced — this function is pure
--   and never decides whether monsters act next.
--
--   Refusal paths (no bow equipped, empty quiver) push a message
--   and leave 'invArrows' untouched, so the caller's "did we
--   shoot?" check reads cleanly. The successful path:
--
--     1. Decrements 'invArrows' by one.
--     2. Walks the ray tile-by-tile via 'walkRay'. The ray
--        starts one tile ahead of the player and is capped at
--        'arrowRange'.
--     3. On 'RayHitMonster', rolls a to-hit + crit via
--        'C.resolveWith' using an "effective" stat block that
--        layers 'Inv.bowRangedBonus' on top of the base attack,
--        then hands the result to 'applyHitResult' — the same
--        kill/loot/victory pipeline 'playerAttack' uses.
--     4. On 'RayBlocked' / 'RayDropped', just logs a message.
fireArrow :: Dir -> GameState -> GameState
fireArrow dir gs =
  let npcHit   p = isJust (npcAt   p (gsNPCs   gs))
      chestHit p = isJust (chestAt p (gsChests gs))
      monsterL p = monsterAt p (gsMonsters gs)
  in case Ranged.resolveShot dir (gsLevel gs) (gsPlayerPos gs)
            monsterL npcHit chestHit
            (gsInventory gs) (gsPlayerStats gs) (gsRng gs) of
        Ranged.ShotRefused msg ->
          pushMsg msg gs
        Ranged.ShotMissed msg ->
          pushMsg msg (decArrow gs)
        Ranged.ShotLanded i m result msg gen' ->
          let gs' = (decArrow gs) { gsRng = gen' }
          in C.applyHitResult gs' i m result [msg]
  where
    pushMsg m s = s { gsMessages = m : gsMessages s }
    decArrow s  =
      let inv = gsInventory s
      in s { gsInventory = inv { invArrows = invArrows inv - 1 } }

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
    Just (IArrows n, rest) ->
      -- Arrow bundles never occupy inventory slots; they stack
      -- straight into 'invArrows'. Picking one up therefore
      -- always succeeds, even on a full bag.
      let inv  = gsInventory gs
          inv' = inv { invArrows = invArrows inv + n }
      in gs { gsInventory    = inv'
            , gsItemsOnFloor = rest
            , gsMessages     =
                ("You pick up the " ++ itemName (IArrows n) ++ ".")
                : gsMessages gs
            }
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
              -- Bump the run-stats counter here: any successful
              -- quaff counts, including ones that healed for 0
              -- (already full HP). The player made the choice
              -- to consume the potion, so it's "used" from the
              -- score-card's point of view.
              , gsPotionsUsed = gsPotionsUsed gs + 1
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
      IArrows _ ->
        -- Arrow bundles live on 'invArrows', not in the bag, so
        -- this case is defensively unreachable — the pickup path
        -- folds them into the counter. Leave a no-op with a
        -- reassurance line in case a stray bundle ever does land
        -- in 'invItems' (e.g. future wizard-spawn).
        gs { gsMessages =
               "You check your quiver and carry on."
               : gsMessages gs
           }
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
  , plChests    = gsChests gs
  , plExplored  = gsExplored gs
  , plPlayerPos = gsPlayerPos gs
  , plBossRoom  = gsBossRoom gs
  }

-- | Swap a 'ParkedLevel' in as the current level. The caller is
--   responsible for removing it from 'gsLevels' if appropriate
--   (so we don't leave a stale parked copy lying around).
loadParked :: ParkedLevel -> GameState -> GameState
loadParked pl gs =
  -- Chests on the floor we're re-entering may have been empty
  -- long enough to refill. Check every chest and re-roll any
  -- whose cooldown already hit zero, threading the main RNG so
  -- the refills are reproducible under save/load.
  let (chests', gen') = Chest.refillChests (gsRng gs) (plChests pl)
  in gs
       { gsLevel        = plLevel pl
       , gsMonsters     = plMonsters pl
       , gsItemsOnFloor = plItems pl
       , gsChests       = chests'
       , gsExplored     = plExplored pl
       , gsPlayerPos    = plPlayerPos pl
       , gsBossRoom     = plBossRoom pl
       , gsRng          = gen'
       -- Any AI-generated flavor text from the floor we're leaving
       -- is no longer relevant — drop it so the panel doesn't
       -- linger into the new (old) floor. The process-local dedup
       -- set in the AI runtime still remembers which rooms have
       -- been described, so the old text is simply not re-fetched.
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
             (regulars, g3)      = spawnMonsters g2 depth regularRooms
             (dragonPos, g4)     = pickBossTopLeft bossRoom g3
             dragon              = mkMonster Dragon dragonPos
             dl                  = D.stripStairsDown dl0
         in gs
              -- Boss floors are deliberately chest-free: the finale
              -- is a pure combat encounter, and an extra potion
              -- cache would undermine the tension.
              { gsLevel        = dl
              , gsMonsters     = dragon : regulars
              , gsItemsOnFloor = keyLoot
              , gsChests       = []
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
             (monsters, g3) = spawnMonsters g2 depth spawnRooms
             -- Chest placement rules (plan: Milestone 17, Step 1B):
             --   * depths 2-3: no chests at all — the middle floors
             --     stay lean so loot feels earned from monster drops
             --     alone.
             --   * depth 4+: 60% chance of 1-2 chests in non-starting
             --     rooms, rolled from the generator RNG so the
             --     outcome is save/load-deterministic.
             occupiedForChests =
                 start
               : concatMap monsterTiles monsters
               ++ map fst keyLoot
             (chests, g4)   =
               if depth >= 4
                 then
                   let (roll, g4a) = randomR (1 :: Int, 100) g3
                   in if roll <= 60
                        then
                          let (n, g4b) = randomR (1 :: Int, 2) g4a
                          in placeChests g4b dl0 (drop 1 rooms) occupiedForChests n
                        else ([], g4a)
                 else ([], g3)
         in gs
              { gsLevel        = dl0
              , gsMonsters     = monsters
              , gsItemsOnFloor = keyLoot
              , gsChests       = chests
              , gsExplored     = Set.empty
              , gsPlayerPos    = start
              , gsRng          = g4
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

-- | Place up to @n@ full chests in the given candidate rooms,
--   avoiding any tile already occupied by a monster (per footprint),
--   item, NPC, another chest, or terrain that isn't a plain floor
--   (stairs, doors, walls). Each chest is seeded with a freshly
--   rolled item from 'Chest.rollChestLoot', so newly generated
--   floors show a full chest until the player bumps it.
--
--   The RNG is threaded through the picks and the loot rolls so
--   the placement is save/load-deterministic. Returns as many
--   chests as could be placed — if every candidate tile is
--   occupied, fewer than @n@ (possibly zero) chests are returned.
placeChests
  :: StdGen
  -> DungeonLevel
  -> [D.Room]
  -> [Pos]
  -> Int
  -> ([Chest], StdGen)
placeChests gen0 dl rooms occupied n
  | n <= 0 || null rooms = ([], gen0)
  | otherwise =
      let candidates =
            [ p
            | r <- rooms
            , p <- roomPositions r
            , case tileAt dl p of
                Just Floor -> True
                _          -> False
            , p `notElem` occupied
            ]
      in pick gen0 candidates n
  where
    pick g _    0 = ([], g)
    pick g []   _ = ([], g)
    pick g cs   k =
      let (i, g1)     = randomR (0, length cs - 1) g
          pos         = cs !! i
          rest        = take i cs ++ drop (i + 1) cs
          (item, g2)  = Chest.rollChestLoot g1
          chest       = Chest { chestPos = pos, chestState = ChestFull item }
          (more, g3)  = pick g2 rest (k - 1)
      in (chest : more, g3)

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
          (quests', qMsgs) = fireQuestEvent (EvEnteredDepth nextDepth) (gsQuests gs'')
      in gs'' { gsQuests = quests', gsMessages = reverse qMsgs ++ gsMessages gs'' }
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

-- | Per-turn player-state tick. Runs once at the top of
--   'processMonsters' (the single "a turn has passed" hook) and
--   advances anything that should decay or accumulate over time
--   without being tied to a specific action. Currently:
--
--     * dash cooldown decrement ('tickDash')
--     * passive HP regen counter ('tickRegen')
--     * global turn counter for run stats ('tickTurnCounter')
--
--   Each rule is a self-contained 'GameState -> GameState'
--   transform; composition order is only significant for
--   'tickTurnCounter' which must run /first/ so 'tickRegen' can
--   see the counter advance exactly once per turn if we ever
--   wire regen to it, and so tests that read 'gsTurnsElapsed'
--   after a single tick see the incremented value regardless of
--   which other rules ran.
tickPlayerTurn :: GameState -> GameState
tickPlayerTurn = tickRegen . tickDash . tickTurnCounter . tickChests

-- | Per-turn chest decay. Walks 'gsChests' and advances every
--   'ChestEmpty' counter one step closer to refilling; 'ChestFull'
--   chests are untouched. Parked floors do NOT receive this tick —
--   'loadParked' instead performs a single bulk refill check on
--   re-entry, which is equivalent to having ticked while away and
--   rolling new loot on arrival.
tickChests :: GameState -> GameState
tickChests gs = gs { gsChests = map Chest.tickChest (gsChests gs) }

-- | Isolated dash-cooldown tick. Split out of 'tickPlayerTurn' so
--   the regen composition stays readable and so tests can cover
--   each rule in isolation.
tickDash :: GameState -> GameState
tickDash gs
  | gsDashCooldown gs > 0 = gs { gsDashCooldown = gsDashCooldown gs - 1 }
  | otherwise             = gs

-- | Passive HP regen: while no hostile monster sits in the
--   player's current FOV, accumulate one "safe turn" per call; on
--   hitting 'regenInterval' add 1 HP (capped at 'sMaxHP') and
--   reset. A hostile becoming visible — or the counter being
--   ticked while a hostile is already visible — immediately
--   resets the counter so the player has to fully disengage
--   before sustain starts again.
--
--   Early-outs:
--     * full HP → no-op (don't burn the counter toward nothing)
--     * dead or victorious → no-op (no further gameplay)
--
--   Hostility is currently a coarse proxy: every 'Monster' on the
--   level is treated as hostile. NPCs live on a separate list
--   (see 'gsNPCs') so they never reach this check.
tickRegen :: GameState -> GameState
tickRegen gs
  | gsDead gs                              = gs
  | gsVictory gs                           = gs
  | sHP (gsPlayerStats gs) >= sMaxHP (gsPlayerStats gs) =
      gs { gsRegenCounter = 0 }
  | hostileVisible                         = gs { gsRegenCounter = 0 }
  | otherwise                              =
      let next = gsRegenCounter gs + 1
      in if next >= regenInterval
           then gs { gsRegenCounter = 0
                   , gsPlayerStats  =
                       let s = gsPlayerStats gs
                       in s { sHP = min (sMaxHP s) (sHP s + 1) }
                   }
           else gs { gsRegenCounter = next }
  where
    vis             = gsVisible gs
    hostileVisible  = any (\m -> any (`Set.member` vis) (monsterTiles m))
                          (gsMonsters gs)

-- | Run-stats clock. Every call advances 'gsTurnsElapsed' by one,
--   unless the run has already ended (death or victory): once
--   'gsFinalTurns' is 'Just', the counter is frozen at its
--   snapshot so the victory modal and HUD keep showing the same
--   number. A dead player's counter also stops — there's no
--   further gameplay, and a permanently-ticking "time of death"
--   number would look strange on the end screen.
tickTurnCounter :: GameState -> GameState
tickTurnCounter gs
  | gsDead gs                       = gs
  | Just _ <- gsFinalTurns gs       = gs
  | otherwise =
      gs { gsTurnsElapsed = gsTurnsElapsed gs + 1 }

-- | Compute a textual "rank" label for a finished run, based on
--   the three gamified counters: how many turns the boss kill
--   took, how many potions were burned, and how many times the
--   player saved. Lower is better on every axis. Intended to be
--   displayed only on the victory modal — on an in-progress or
--   dead run this still returns a string ("In Progress") so the
--   caller doesn't have to special-case the Maybe.
--
--   Thresholds are deliberately generous: the goal is to reward
--   the player for a clean sprint, not to make most runs look
--   bad. Tiers and their intent:
--
--     * /Legendary/ — genuine speedrun: ≤ 1500 turns, ≤ 3
--       potions, 0 saves. Hard to hit by accident.
--     * /Heroic/ — disciplined run: ≤ 2500 turns, ≤ 6 potions,
--       ≤ 2 saves. The "most players who finish" bucket.
--     * /Victor/ — you finished. Shown on any victory that
--       doesn't clear the looser bar.
--
--   Ranges are exclusive of the lower tier by construction — a
--   run that qualifies for Legendary trivially qualifies for
--   Heroic / Victor, and we pick the strictest matching label.
runRank :: GameState -> String
runRank gs = case gsFinalTurns gs of
  Nothing -> if gsDead gs then "Fallen" else "In Progress"
  Just t
    | t <= 1500 && gsPotionsUsed gs <= 3 && gsSavesUsed gs == 0 -> "Legendary"
    | t <= 2500 && gsPotionsUsed gs <= 6 && gsSavesUsed gs <= 2 -> "Heroic"
    | otherwise                                                 -> "Victor"

processMonsters :: GameState -> GameState
processMonsters gs0 = go (tickPlayerTurn gs0) 0
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
      evs             = case C.monsterCombatEvent result of
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
