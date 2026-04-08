module Game.Render
  ( drawGame
  , fogAttr
  , npcAttr
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
import Game.Logic.Quest
  ( Quest(..), QuestStatus(..), questDescription, questProgressLabel )
import Game.Types

-- | Attribute name used for "explored but not currently visible"
--   tiles. Main wires this to a dim color in its 'attrMap'.
fogAttr :: AttrName
fogAttr = attrName "fog"

-- | Attribute name used for NPC glyphs on the map. Main wires this
--   to a yellow foreground so NPCs read as friendly and stand out
--   from monsters.
npcAttr :: AttrName
npcAttr = attrName "npc"

drawGame :: GameState -> [Widget ()]
drawGame gs =
  let baseLayer = vBox
        [ drawGrid gs
        , drawPromptLine gs
        , drawStatus gs
        , drawQuests gs
        , drawMessages gs
        , str "Move: arrows/hjkl/yubn  Wait: .  Get: g  Inv: i  Quests: Q  Cmd: /  Quit: q/Esc"
        ]
  in case gsDialogue gs of
       Just i | Just npc <- nthMaybe i (gsNPCs gs) ->
         [drawDialogueModal npc, baseLayer]
       _
         | gsConfirmQuit  gs  -> [drawQuitConfirmModal, baseLayer]
         | gsQuestLogOpen gs  -> [drawQuestLogModal gs, baseLayer]
         | gsInventoryOpen gs -> [drawInventoryModal gs, baseLayer]
         | otherwise          -> [baseLayer]
  where
    nthMaybe n xs
      | n < 0 || n >= length xs = Nothing
      | otherwise               = Just (xs !! n)

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
      visibleCell gs pos
  | pos `Set.member` exp_ =
      withAttr fogAttr $ str [tileGlyph (gsLevel gs) pos]
  | otherwise =
      str " "

-- | Render the cell at a visible position, with its glyph and (if
--   applicable) attribute. Priority from top to bottom: player,
--   monster, NPC, item on floor, terrain.
visibleCell :: GameState -> Pos -> Widget ()
visibleCell gs pos
  | pos == gsPlayerPos gs = str "@"
  | Just m <- find (\mo -> mPos mo == pos) (gsMonsters gs) =
      str [monsterGlyph (mKind m)]
  | Just _ <- find (\n -> npcPos n == pos) (gsNPCs gs) =
      withAttr npcAttr $ str "N"
  | Just (_, it) <- find (\(p, _) -> p == pos) (gsItemsOnFloor gs) =
      str [itemGlyph it]
  | otherwise =
      str [tileGlyph (gsLevel gs) pos]

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

-- | One-line quest panel: each active quest is shown as
--   @"Name p/target"@, separated by two spaces. If there are no
--   quests (shouldn't normally happen), renders an empty line so
--   the layout doesn't jump.
drawQuests :: GameState -> Widget ()
drawQuests gs = case gsQuests gs of
  [] -> str " "
  qs -> str $ "Quests: " ++ intercalateTwo (map fmt qs)
  where
    fmt q = qName q ++ " " ++ questProgressLabel q
    intercalateTwo []     = ""
    intercalateTwo [x]    = x
    intercalateTwo (x:xs) = x ++ "  " ++ intercalateTwo xs

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

-- | Centered modal showing an NPC's greeting and the list of
--   quests they currently have to offer. Letter keys @a@..@z@
--   accept the corresponding offer; @Esc@ closes the modal
--   without accepting anything (which leaves the offers on the
--   NPC so the player can come back later).
drawDialogueModal :: NPC -> Widget ()
drawDialogueModal npc =
  let header =
        [ "\"" ++ npcGreeting npc ++ "\""
        , ""
        , "Quests on offer:"
        ]
      offerLines = case npcOffers npc of
        [] -> ["  (none — you've taken them all)"]
        xs ->
          [ "  " ++ [letter] ++ ") " ++ qName q ++ " — " ++ questDescription q
          | (letter, q) <- zip ['a' ..] xs
          ]
      footer =
        [ ""
        , "[letter] accept   Esc close"
        ]
      body = vBox $ map str (header ++ offerLines ++ footer)
      label = " " ++ npcName npc ++ " "
  in centerLayer $ borderWithLabel (str label) $ padAll 1 body

-- | Full quest journal. Shows Active, Completed, and Failed
--   sections. Active quests are labeled @a@..@z@; pressing a
--   letter selects a quest, pressing @x@ while selected marks it
--   abandoned (two keystrokes serve as a built-in confirm). The
--   selected quest's letter is decorated with an asterisk so the
--   player can see what they're about to abandon.
drawQuestLogModal :: GameState -> Widget ()
drawQuestLogModal gs =
  let qs         = gsQuests gs
      active     = [ q | q <- qs, qStatus q == QuestActive ]
      completed  = [ q | q <- qs, qStatus q == QuestCompleted ]
      failed     = [ q | q <- qs, qStatus q == QuestFailed ]
      cursor     = gsQuestLogCursor gs

      section title items fmt =
        let rows = case items of
              [] -> ["  (none)"]
              xs -> map fmt (zip [(0 :: Int) ..] xs)
        in (title ++ ":") : rows

      activeLine (idx, q) =
        let letter     = toEnum (fromEnum 'a' + idx) :: Char
            marker     = case cursor of
              Just c | c == idx -> '*'
              _                 -> ' '
            progress   = questProgressLabel q
        in "  " ++ [marker] ++ [letter] ++ ") "
             ++ qName q ++ "  [" ++ progress ++ "]  —  "
             ++ questDescription q

      doneLine (_, q) =
        "  - " ++ qName q ++ "  —  " ++ questDescription q

      lines_ =
        section "Active"    active    activeLine
        ++ [""]
        ++ section "Completed" completed doneLine
        ++ [""]
        ++ section "Failed"    failed    doneLine
        ++ [""]
        ++ [ "[letter] select   x abandon selected   Esc close" ]

      body = vBox (map str lines_)
  in centerLayer $ borderWithLabel (str " Quest Log ") $ padAll 1 body

-- | A tiny confirmation modal shown when the player presses @q@
--   or @Esc@ in normal mode. Prevents fat-fingered quits given
--   that @q@ (Quit) and @Q@ (Quest Log) are one shift-key apart.
drawQuitConfirmModal :: Widget ()
drawQuitConfirmModal =
  centerLayer
    $ borderWithLabel (str " Quit? ")
    $ padAll 1
    $ vBox
        [ str "Really quit this run?"
        , str ""
        , str "  y : yes, quit"
        , str "  n / Esc : keep playing"
        ]
