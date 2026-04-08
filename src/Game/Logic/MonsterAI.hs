module Game.Logic.MonsterAI
  ( MonsterIntent(..)
  , monsterIntent
  , chebyshev
  ) where

import Linear (V2(..))

import Game.Types

-- | What a monster wants to do this turn.
data MonsterIntent
  = MiWait
  | MiMove !Pos
  | MiAttack
  deriving (Eq, Show)

chebyshev :: Pos -> Pos -> Int
chebyshev (V2 x1 y1) (V2 x2 y2) = max (abs (x1 - x2)) (abs (y1 - y2))

-- | Decide what a monster should do this turn. Pure.
--
--   If adjacent to the player, attack. Otherwise pick the walkable
--   neighbor that gets closest to the player, avoiding tiles in the
--   'blockedPositions' list (other monsters).
monsterIntent :: DungeonLevel -> Pos -> [Pos] -> Pos -> MonsterIntent
monsterIntent dl playerPos blockedPositions mpos
  | chebyshev mpos playerPos <= 1 = MiAttack
  | otherwise =
      let candidates =
            [ p
            | d <- [N, NE, E, SE, S, SW, W, NW]
            , let p = mpos + dirToOffset d
            , p /= playerPos
            , p `notElem` blockedPositions
            , case tileAt dl p of
                Just t  -> isWalkable t
                Nothing -> False
            ]
      in case bestNeighbor playerPos candidates of
           Just p  -> MiMove p
           Nothing -> MiWait

bestNeighbor :: Pos -> [Pos] -> Maybe Pos
bestNeighbor target = foldl' pick Nothing
  where
    pick Nothing  p = Just p
    pick (Just b) p
      | chebyshev p target < chebyshev b target = Just p
      | otherwise                               = Just b
