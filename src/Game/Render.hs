module Game.Render (drawGame) where

import Brick
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
      , str "Move: arrows / hjkl / yubn   Wait: .   Quit: q / Esc"
      ]
  ]

-- | Render the dungeon as one 'str' widget per row. This is the idiomatic
--   Brick approach for fixed-character grids and lets Brick's diffing
--   handle cell updates correctly across frames.
drawGrid :: GameState -> Widget ()
drawGrid gs =
  let dl = gsLevel gs
      p  = gsPlayerPos gs
      rowStr y = [ cellChar p (V2 x y) dl | x <- [0 .. dlWidth dl - 1] ]
  in vBox [ str (rowStr y) | y <- [0 .. dlHeight dl - 1] ]

cellChar :: Pos -> Pos -> DungeonLevel -> Char
cellChar playerPos pos dl
  | pos == playerPos = '@'
  | otherwise = case tileAt dl pos of
      Just Floor         -> '.'
      Just Wall          -> '#'
      Just (Door Open)   -> '/'
      Just (Door Closed) -> '+'
      Just StairsDown    -> '>'
      Just StairsUp      -> '<'
      Nothing            -> ' '
