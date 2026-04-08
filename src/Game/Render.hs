module Game.Render (drawGame) where

import Brick
import qualified Graphics.Vty as V
import Linear (V2(..))

import Game.GameState
import Game.Types

drawGame :: GameState -> [Widget ()]
drawGame gs =
  [ vBox
      [ raw (gridImage gs)
      , str " "
      , str "Move: arrows / hjkl / yubn   Wait: .   Quit: q / Esc"
      ]
  ]

-- | Build the whole dungeon grid as a single Vty 'Image'.
--
--   Going through a single pre-built 'V.Image' (instead of one tiny
--   'Widget' per cell) avoids layout weirdness and gives pixel-stable
--   output — exactly what we want for a fixed character grid.
gridImage :: GameState -> V.Image
gridImage gs =
  let dl    = gsLevel gs
      ppos  = gsPlayerPos gs
      rowStr y = [ cellChar ppos (V2 x y) dl | x <- [0 .. dlWidth dl - 1] ]
      rowImg y = V.string V.defAttr (rowStr y)
  in V.vertCat [ rowImg y | y <- [0 .. dlHeight dl - 1] ]

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
