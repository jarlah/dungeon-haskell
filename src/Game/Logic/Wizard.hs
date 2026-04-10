-- | Pure implementations of wizard/debug commands. Each function
--   takes the specific data it needs and returns results that the
--   caller wires back into 'GameState'.
module Game.Logic.Wizard
  ( wizCmdReveal
  , wizCmdHeal
  , wizCmdKillAll
  , wizCmdTeleport
  , wizCmdSpawn
  , wizCmdXP
  ) where

import qualified Data.Set as Set
import Data.Set (Set)
import Linear (V2(..))

import Game.Types
  ( Pos, DungeonLevel(..), Monster(..), MonsterKind, Stats(..)
  , GameEvent(..), mkMonster, isWalkable, tileAt, dirToOffset
  )
import qualified Game.Logic.Progression as P

-- | Reveal every tile on the map. Returns the new explored set.
wizCmdReveal :: DungeonLevel -> Set Pos -> Set Pos
wizCmdReveal dl explored =
  let all_ = Set.fromList
        [ V2 x y
        | x <- [0 .. dlWidth  dl - 1]
        , y <- [0 .. dlHeight dl - 1]
        ]
  in Set.union explored all_

-- | Fully heal the player. Returns updated stats.
wizCmdHeal :: Stats -> Stats
wizCmdHeal s = s { sHP = sMaxHP s }

-- | Banish all monsters. Returns (kill count, empty list).
wizCmdKillAll :: [Monster] -> (Int, [Monster])
wizCmdKillAll ms = (length ms, [])

-- | Teleport if the target tile is walkable and in-bounds.
--   Returns @Right newPos@ on success or @Left errorMsg@ on failure.
wizCmdTeleport :: Pos -> DungeonLevel -> Either String Pos
wizCmdTeleport p dl =
  case tileAt dl p of
    Just t | isWalkable t -> Right p
    Just _ -> Left ("tile at " ++ showPos p ++ " is blocked.")
    Nothing -> Left (showPos p ++ " is outside the map.")
  where
    showPos (V2 x y) = "(" ++ show x ++ "," ++ show y ++ ")"

-- | Spawn a monster on the first walkable unoccupied tile adjacent
--   to the player. Returns @Right updatedMonsters@ or @Left errorMsg@.
wizCmdSpawn :: MonsterKind -> Pos -> DungeonLevel -> [Monster] -> Either String [Monster]
wizCmdSpawn kind playerPos dl monsters =
  let neighbors = [ playerPos + dirToOffset d | d <- [minBound .. maxBound] ]
      occupied  = Set.fromList (map mPos monsters)
      free =
        [ p
        | p <- neighbors
        , case tileAt dl p of
            Just t  -> isWalkable t
            Nothing -> False
        , not (Set.member p occupied)
        ]
  in case free of
       []      -> Left "no room to spawn next to you."
       (p : _) -> Right (monsters ++ [mkMonster kind p])

-- | Grant XP and compute level-up messages and events.
--   Returns @(newStats, levelMsgs, levelEvents)@.
wizCmdXP :: Int -> Stats -> Either String (Stats, [String], [GameEvent])
wizCmdXP n stats
  | n < 0 = Left "XP must be non-negative."
  | otherwise =
      let (s', ups)  = P.gainXP stats n
          startLvl   = sLevel stats
          endLvl     = sLevel s'
          lvlMsgs    =
            [ "You reach level " ++ show l ++ "!"
            | l <- [endLvl, endLvl - 1 .. startLvl + 1]
            ]
      in Right (s', lvlMsgs, replicate ups EvLevelUp)
