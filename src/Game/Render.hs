module Game.Render (drawGame) where

import Brick
import Data.List (find)
import Linear (V2(..))

import Game.GameState
import Game.Types

drawGame :: GameState -> [Widget ()]
drawGame gs =
  [ vBox
      [ drawGrid gs
      -- Park the terminal cursor on this blank spacer line. Some
      -- terminals don't honor Vty's hide-cursor escape reliably, so
      -- we explicitly pin it to a stable, visually quiet spot.
      , showCursor () (Location (0, 0)) $ str " "
      , drawStatus gs
      , drawMessages gs
      , str "Move: arrows / hjkl / yubn   Wait: .   Quit: q / Esc"
      ]
  ]

-- | Render the dungeon as one 'str' widget per row.
drawGrid :: GameState -> Widget ()
drawGrid gs =
  let dl       = gsLevel gs
      p        = gsPlayerPos gs
      monsters = gsMonsters gs
      rowStr y = [ cellChar p monsters (V2 x y) dl
                 | x <- [0 .. dlWidth dl - 1] ]
  in vBox [ str (rowStr y) | y <- [0 .. dlHeight dl - 1] ]

cellChar :: Pos -> [Monster] -> Pos -> DungeonLevel -> Char
cellChar playerPos monsters pos dl
  | pos == playerPos = '@'
  | otherwise = case find (\m -> mPos m == pos) monsters of
      Just m  -> monsterGlyph (mKind m)
      Nothing -> case tileAt dl pos of
        Just Floor         -> '.'
        Just Wall          -> '#'
        Just (Door Open)   -> '/'
        Just (Door Closed) -> '+'
        Just StairsDown    -> '>'
        Just StairsUp      -> '<'
        Nothing            -> ' '

drawStatus :: GameState -> Widget ()
drawStatus gs =
  let s      = gsPlayerStats gs
      dl     = gsLevel gs
      status = "HP: "   ++ show (sHP s)     ++ "/" ++ show (sMaxHP s)
            ++ "   ATK: " ++ show (sAttack s)
            ++ "   DEF: " ++ show (sDefense s)
            ++ "   Depth: " ++ show (dlDepth dl)
            ++ (if gsDead gs then "   *** YOU DIED ***" else "")
  in str status

drawMessages :: GameState -> Widget ()
drawMessages gs = vBox (map str (take 3 (gsMessages gs)))
