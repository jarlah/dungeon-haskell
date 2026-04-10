-- | Entity spawning for freshly generated dungeon levels. All
--   functions are pure — they thread 'StdGen' explicitly and never
--   touch 'GameState'.
module Game.Logic.Spawning
  ( spawnMonsters
  , spawnInRoom
  , randomMonsterKind
  , randomRoomPos
  , spawnNPCs
  , mkOffer
  , acceptOffer
  ) where

import Linear (V2(..))
import System.Random (StdGen, randomR)

import Game.Types (Pos, Monster, MonsterKind(..), mkMonster)
import Game.State.Types (NPC(..))
import qualified Game.Logic.Dungeon as D
import Game.Logic.Quest (Quest(..), QuestGoal(..), QuestStatus(..), mkQuest)

-- | Build a quest in 'QuestNotStarted' state — an offer the NPC
--   has on the table. It sits on the NPC's 'npcOffers' list
--   until the player accepts — at which point the status flips to
--   'QuestActive' via 'acceptOffer'.
mkOffer :: String -> QuestGoal -> Quest
mkOffer name goal = (mkQuest name goal) { qStatus = QuestNotStarted }

-- | Flip an offered quest into an accepted one.
acceptOffer :: Quest -> Quest
acceptOffer q = q { qStatus = QuestActive }

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
