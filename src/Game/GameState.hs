module Game.GameState
  ( GameState(..)
  , mkGameState
  , newGame
  , defaultPlayerStats
  , hardcodedRoom
  , hardcodedInitialState
  , applyAction
  , fovRadius
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Vector as V
import Linear (V2(..))
import System.Random (StdGen, mkStdGen, randomR)

import Game.Types
import qualified Game.Logic.Combat as C
import Game.Logic.Combat (Damage(..))
import qualified Game.Logic.Dungeon as D
import qualified Game.Logic.FOV as FOV
import Game.Logic.MonsterAI (MonsterIntent(..), monsterIntent)
import qualified Game.Logic.Movement as M
import qualified Game.Logic.Progression as P

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
  } deriving (Show)

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
  }

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
  in mkGameState gen2 dl startPos monsters

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

applyAction :: GameAction -> GameState -> GameState
applyAction act gs0 =
  -- Each action starts with a fresh event log so consumers (audio)
  -- only see what happened on *this* turn.
  let gs = gs0 { gsEvents = [] }
  in recomputeVisibility $ case act of
       Quit                -> gs { gsQuitting = True }
       _ | gsDead gs       -> gs
       Wait                -> processMonsters gs
       Move dir            ->
         let target = gsPlayerPos gs + dirToOffset dir
         in case monsterAt target (gsMonsters gs) of
              Just (i, m) -> processMonsters (playerAttack gs i m)
              Nothing     ->
                case M.tryMove (gsLevel gs) (gsPlayerPos gs) dir of
                  Just newPos -> processMonsters (gs { gsPlayerPos = newPos })
                  Nothing     -> gs  -- blocked; turn does not advance

-- | Append events to the running per-turn log.
emit :: GameState -> [GameEvent] -> GameState
emit gs evs = gs { gsEvents = gsEvents gs ++ evs }

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

playerAttack :: GameState -> Int -> Monster -> GameState
playerAttack gs i m =
  let (result, gen') = C.resolveAttack (gsRng gs) (gsPlayerStats gs) (mStats m)
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
  in emit
       gs
         { gsMonsters    = monsters'
         , gsPlayerStats = playerStats'
         , gsRng         = gen'
         , gsMessages    = levelMsgs ++ [msg] ++ gsMessages gs
         }
       (combatEv : levelEvs)

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
  let (result, gen')  = C.resolveAttack (gsRng gs) (mStats m) (gsPlayerStats gs)
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
