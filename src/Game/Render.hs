module Game.Render
  ( drawGame
  , fogAttr
  ) where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer)
import Data.List (find)
import qualified Data.Set as Set
import Data.Set (Set)
import Linear (V2(..))

import Game.GameState
import qualified Game.Logic.Inventory as Inv
import Game.Logic.Progression (xpForNextLevel)
import Game.Types

-- | Attribute name used for "explored but not currently visible"
--   tiles. Main wires this to a dim color in its 'attrMap'.
fogAttr :: AttrName
fogAttr = attrName "fog"

drawGame :: GameState -> [Widget ()]
drawGame gs =
  let baseLayer = vBox
        [ drawGrid gs
        , drawPromptLine gs
        , drawStatus gs
        , drawMessages gs
        , str "Move: arrows / hjkl / yubn   Wait: .   Get: g   Inv: i   Cmd: /   Quit: q / Esc"
        ]
  in if gsInventoryOpen gs
       then [drawInventoryModal gs, baseLayer]
       else [baseLayer]

-- | The line between the map and the status bar. When the
--   slash-command prompt is closed it's a blank spacer that holds
--   the terminal cursor (some terminals ignore hide-cursor
--   escapes, so we keep it visually quiet). When the prompt is
--   open it shows @> buf@ with the cursor parked after the buffer.
drawPromptLine :: GameState -> Widget ()
drawPromptLine gs = case gsPrompt gs of
  Nothing  ->
    showCursor () (Location (0, 0)) $ str " "
  Just buf ->
    let line = "> " ++ buf
    in showCursor () (Location (length line, 0)) $ str line

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

-- | Glyph for a visible tile. Priority from top to bottom:
--   player, monster, item on floor, terrain.
visibleGlyph :: GameState -> Pos -> Char
visibleGlyph gs pos
  | pos == gsPlayerPos gs = '@'
  | otherwise = case find (\m -> mPos m == pos) (gsMonsters gs) of
      Just m  -> monsterGlyph (mKind m)
      Nothing -> case find (\(p, _) -> p == pos) (gsItemsOnFloor gs) of
        Just (_, it) -> itemGlyph it
        Nothing      -> tileGlyph (gsLevel gs) pos

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
  let s       = gsPlayerStats gs
      effStats = Inv.effectiveStats s (gsInventory gs)
      dl      = gsLevel gs
      status  = "LVL "   ++ show (sLevel s)
             ++ "   XP: "  ++ show (sXP s) ++ "/" ++ show (xpForNextLevel (sLevel s))
             ++ "   HP: "  ++ show (sHP s) ++ "/" ++ show (sMaxHP s)
             ++ "   ATK: " ++ show (sAttack effStats)
             ++ "   DEF: " ++ show (sDefense effStats)
             ++ "   Depth: " ++ show (dlDepth dl)
             ++ (if gsDead gs then "   *** YOU DIED ***" else "")
  in str status

drawMessages :: GameState -> Widget ()
drawMessages gs = vBox (map str (take 3 (gsMessages gs)))

-- | Centered modal listing the player's inventory. Bag items are
--   lettered @a@, @b@, @c@, ... and pressing the corresponding key
--   while the modal is open applies the item's default action
--   (quaff a potion, equip a weapon or armor).
drawInventoryModal :: GameState -> Widget ()
drawInventoryModal gs =
  let inv    = gsInventory gs
      header =
        [ "Equipped:"
        , "  Weapon: " ++ maybe "(none)" (itemName . IWeapon) (invWeapon inv)
        , "  Armor:  " ++ maybe "(none)" (itemName . IArmor)  (invArmor  inv)
        , ""
        , "Bag (" ++ show (Inv.inventoryCount inv) ++ "/" ++ show Inv.invCapacity ++ "):"
        ]
      bagLines = case invItems inv of
        [] -> ["  (empty)"]
        xs ->
          [ "  " ++ [letter] ++ ") " ++ itemName it
          | (letter, it) <- zip ['a' ..] xs
          ]
      footer =
        [ ""
        , "[letter] use/equip   Esc close"
        ]
      body = vBox $ map str (header ++ bagLines ++ footer)
  in centerLayer $ borderWithLabel (str " Inventory ") $ padAll 1 body
