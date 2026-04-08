module Game.Render
  ( drawGame
  , fogAttr
  ) where

import Brick
import Data.List (find)
import qualified Data.Set as Set
import Data.Set (Set)
import Linear (V2(..))

import Game.GameState
import Game.Logic.Progression (xpForNextLevel)
import Game.Types

-- | Attribute name used for "explored but not currently visible"
--   tiles. Main wires this to a dim color in its 'attrMap'.
fogAttr :: AttrName
fogAttr = attrName "fog"

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

-- | Render the dungeon one row at a time. Each cell is classified
--   into one of three states: visible (normal), explored-but-not-
--   visible (dim fog), or unseen (blank).
drawGrid :: GameState -> Widget ()
drawGrid gs =
  let dl  = gsLevel gs
      vis = gsVisible  gs
      exp_ = gsExplored gs
  in vBox
       [ hBox
           [ drawCell gs vis exp_ (V2 x y)
           | x <- [0 .. dlWidth dl - 1] ]
       | y <- [0 .. dlHeight dl - 1] ]

drawCell :: GameState -> Set Pos -> Set Pos -> Pos -> Widget ()
drawCell gs vis exp_ pos
  | pos `Set.member` vis =
      str [visibleGlyph gs pos]
  | pos `Set.member` exp_ =
      withAttr fogAttr $ str [tileGlyph (gsLevel gs) pos]
  | otherwise =
      str " "

-- | Glyph for a visible tile: player > monster > terrain.
visibleGlyph :: GameState -> Pos -> Char
visibleGlyph gs pos
  | pos == gsPlayerPos gs = '@'
  | otherwise = case find (\m -> mPos m == pos) (gsMonsters gs) of
      Just m  -> monsterGlyph (mKind m)
      Nothing -> tileGlyph (gsLevel gs) pos

-- | Glyph for the terrain at a position, without the player or
--   monsters overlaid. Used for both visible and fogged rendering.
tileGlyph :: DungeonLevel -> Pos -> Char
tileGlyph dl pos = case tileAt dl pos of
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
      status = "LVL "   ++ show (sLevel s)
            ++ "   XP: " ++ show (sXP s) ++ "/" ++ show (xpForNextLevel (sLevel s))
            ++ "   HP: " ++ show (sHP s) ++ "/" ++ show (sMaxHP s)
            ++ "   ATK: " ++ show (sAttack s)
            ++ "   DEF: " ++ show (sDefense s)
            ++ "   Depth: " ++ show (dlDepth dl)
            ++ (if gsDead gs then "   *** YOU DIED ***" else "")
  in str status

drawMessages :: GameState -> Widget ()
drawMessages gs = vBox (map str (take 3 (gsMessages gs)))
