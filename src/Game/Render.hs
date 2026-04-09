module Game.Render
  ( drawGame
  , fogAttr
  , npcAttr
  , bossAttr
  , saveMenuCursorAttr
  , saveMenuEmptyAttr
  , launchCursorAttr
  , launchDisabledAttr
  , launchTitleAttr
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
import Game.Save.Types (SaveMetadata(..))
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

-- | Attribute name used for boss monsters (currently the dragon).
--   Main wires this to a red foreground so a boss's 2x2 footprint
--   reads as a single distinctive mass instead of a grid of
--   normal monster glyphs.
bossAttr :: AttrName
bossAttr = attrName "boss"

-- | Attribute name for the highlighted cursor row in the save/load
--   modal. Main wires this to a reverse-video attribute so the
--   selected row stands out regardless of terminal color support.
saveMenuCursorAttr :: AttrName
saveMenuCursorAttr = attrName "saveMenuCursor"

-- | Attribute name for an empty slot row in the save/load modal
--   when shown in load mode (where empty slots are non-selectable).
--   Main wires this to a dim foreground.
saveMenuEmptyAttr :: AttrName
saveMenuEmptyAttr = attrName "saveMenuEmpty"

-- | Attribute name for the highlighted option on the launch /
--   title screen. Main wires this to reverse-video so the current
--   selection stands out independently of terminal color support.
launchCursorAttr :: AttrName
launchCursorAttr = attrName "launchCursor"

-- | Attribute name for a disabled launch-menu option (for example,
--   Continue / Load when no saves exist). Main wires this to a
--   dim foreground so the row is visible but clearly inert.
launchDisabledAttr :: AttrName
launchDisabledAttr = attrName "launchDisabled"

-- | Attribute name for the game-title banner on the launch screen.
--   Main wires this to a bright color so the title reads as the
--   focal point of the screen.
launchTitleAttr :: AttrName
launchTitleAttr = attrName "launchTitle"

drawGame :: GameState -> [Widget ()]
drawGame gs =
  let baseLayer = vBox
        [ drawGrid gs
        , drawPromptLine gs
        , drawStatus gs
        , drawQuests gs
        , drawMessages gs
        , str "? for help    q/Esc to quit"
        ]
  in case gsLaunchMenu gs of
       -- Launch / title screen swallows the whole viewport so no
       -- gameplay artifacts leak through before the player picks
       -- New Game / Continue / Load / Quit.
       Just lm -> [drawLaunchMenu gs lm]
       Nothing -> case gsDialogue gs of
         Just i | Just npc <- nthMaybe i (gsNPCs gs) ->
           [drawDialogueModal (gsQuests gs) i npc, baseLayer]
         _
           | gsVictory      gs  -> [drawVictoryModal, baseLayer]
           | gsConfirmQuit  gs  -> [drawQuitConfirmModal, baseLayer]
           | gsHelpOpen     gs  -> [drawHelpModal, baseLayer]
           | Just sm <- gsSaveMenu gs -> [drawSaveMenuModal sm, baseLayer]
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
--   monster, NPC, item on floor, terrain. Multi-tile bosses paint
--   their glyph on every tile of their footprint — so a 2x2
--   dragon shows up as a 2x2 block of @D@.
visibleCell :: GameState -> Pos -> Widget ()
visibleCell gs pos
  | pos == gsPlayerPos gs = str "@"
  | Just m <- find (`monsterOccupies` pos) (gsMonsters gs) =
      let attr = if isBoss (mKind m) then bossAttr else mempty
      in withAttr attr $ str [monsterGlyph (mKind m)]
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

-- | Centered modal showing an NPC's greeting, any quests the
--   player is ready to turn in here, and the list of quests this
--   NPC currently has to offer.
--
--   * Capital letters @A@..@Z@ hand in a ready quest — a @*@ next
--     to the entry means this NPC is the original giver and will
--     pay the full bounty; no @*@ means they'll pay half.
--   * Lowercase letters @a@..@z@ accept a new offer.
--   * @Esc@ closes without any choice, leaving both lists intact.
drawDialogueModal :: [Quest] -> Int -> NPC -> Widget ()
drawDialogueModal quests npcIdx npc =
  let ready = [ q | q <- quests, qStatus q == QuestReadyToTurnIn ]
      -- Prefer the AI-generated greeting when we have one cached;
      -- fall back to the hardcoded line otherwise. This is the only
      -- place the two fields are disambiguated — everyone else just
      -- sees "the greeting for this NPC".
      greetingLine = case npcAIGreet npc of
        Just g  -> g
        Nothing -> npcGreeting npc
      header =
        [ "\"" ++ greetingLine ++ "\""
        , ""
        ]
      readySection = case ready of
        [] -> []
        xs ->
          "Ready to turn in:" :
          [ "  " ++ [letter] ++ marker ++ ") "
              ++ qName q ++ " — " ++ rewardNote q
          | (letter, q) <- zip ['A' ..] xs
          , let marker = if qGiver q == Just npcIdx then "*" else " "
          ] ++ [""]
      offerSection =
        "Quests on offer:" :
        case npcOffers npc of
          [] -> ["  (none — you've taken them all)"]
          xs ->
            [ "  " ++ [letter] ++ ") " ++ qName q ++ " — " ++ questDescription q
            | (letter, q) <- zip ['a' ..] xs
            ]
      footer =
        [ ""
        , "[A-Z] hand in  [a-z] accept  Esc close"
        ]
      body = vBox $ map str (header ++ readySection ++ offerSection ++ footer)
      label = " " ++ npcName npc ++ " "
  in centerLayer $ borderWithLabel (str label) $ padAll 1 body
  where
    rewardNote q
      | qGiver q == Just npcIdx =
          "full reward " ++ show (qReward q) ++ " XP"
      | otherwise =
          "partial reward " ++ show (qReward q `div` 2) ++ " XP"

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

-- | Save / load picker modal. Renders the slot list as a vertical
--   column, one row per slot, with the currently-highlighted row
--   drawn in reverse video ('saveMenuCursorAttr'). In save mode
--   every slot is selectable — an empty slot says @(empty)@ and
--   overwriting an existing one kicks the confirm path. In load
--   mode empty slots are greyed out ('saveMenuEmptyAttr') and
--   non-selectable.
--
--   The confirm overlay ("Overwrite slot N? y/n") is drawn in the
--   bottom hint area instead of a second centered modal so the
--   underlying slot list stays on-screen and the player can see
--   what they're about to overwrite.
drawSaveMenuModal :: SaveMenu -> Widget ()
drawSaveMenuModal sm =
  let title = case smMode sm of
        SaveMode -> " Save Game "
        LoadMode -> " Load Game "
      rows = zipWith (drawRow (smMode sm) (smCursor sm)) [0 ..] (smSlots sm)
      hint = case (smMode sm, smConfirm sm) of
        (_, True) ->
          str "Overwrite this slot?   y : yes   n / Esc : cancel"
        (SaveMode, False) ->
          str "[a-z] select slot   x delete   Esc close"
        (LoadMode, False) ->
          str "[a-z] load   x delete   Esc close"
      body = vBox $ rows ++ [str "", hint]
  in centerLayer $ borderWithLabel (str title) $ padAll 1 body
  where
    drawRow :: SaveMenuMode -> Int -> Int -> SaveMenuEntry -> Widget ()
    drawRow mode cursor idx entry =
      let letter  = toEnum (fromEnum 'a' + idx) :: Char
          slotLbl = sseSlotLabel entry
          summary = case sseMeta entry of
            Just md ->
              "depth " ++ show (smDepth md)
              ++ "  lvl " ++ show (smPlayerLvl md)
              ++ "  hp " ++ show (smPlayerHP md)
            Nothing -> "(empty)"
          line    = "  " ++ [letter] ++ ") "
                    ++ padRightStr 8 slotLbl ++ "  —  " ++ summary
          styled =
            case (mode, sseMeta entry) of
              (LoadMode, Nothing) -> withAttr saveMenuEmptyAttr (str line)
              _                   -> str line
      in if idx == cursor
           then withAttr saveMenuCursorAttr styled
           else styled

    padRightStr :: Int -> String -> String
    padRightStr n s = s ++ replicate (max 0 (n - length s)) ' '

-- | Full-screen title / launch screen shown before the player has
--   chosen an entry point (New Game / Continue / Load / Quit). Uses
--   a centered bordered panel so it feels intentional even at very
--   wide terminal sizes. Disabled options (Continue / Load with no
--   saves present) are dimmed via 'launchDisabledAttr' so the
--   player can still see them but knows they won't respond.
drawLaunchMenu :: GameState -> LaunchMenu -> Widget ()
drawLaunchMenu _ lm =
  centerLayer
    $ borderWithLabel (str " Dungeon Haskell ")
    $ padAll 2
    $ vBox
        ( titleBanner
          ++ [str ""]
          ++ zipWith renderOption [0 ..] launchOptions
          ++ [str "", hint]
        )
  where
    cursor   = lmCursor lm
    hasSaves = lmHasSaves lm

    titleBanner =
      [ withAttr launchTitleAttr (str "      D U N G E O N   H A S K E L L      ")
      , str "       a small terminal roguelike         "
      ]

    isEnabled opt = case opt of
      LaunchNewGame  -> True
      LaunchContinue -> hasSaves
      LaunchLoad     -> hasSaves
      LaunchQuit     -> True

    optionLabel opt = case opt of
      LaunchNewGame  -> "New Game"
      LaunchContinue -> "Continue   (most recent save)"
      LaunchLoad     -> "Load Game  (pick a slot)"
      LaunchQuit     -> "Quit"

    renderOption idx opt =
      let marker = if idx == cursor then " > " else "   "
          line   = marker ++ optionLabel opt
          base   = str line
          styled
            | not (isEnabled opt) = withAttr launchDisabledAttr base
            | otherwise           = base
      in if idx == cursor
           then withAttr launchCursorAttr styled
           else styled

    hint = str "  ↑/↓ select    Enter confirm    q / Esc quit"

-- | Shown when the player lands the killing blow on the dragon.
--   Freezes the game (gameplay input is ignored while gsVictory is
--   true) so the only thing the player can do is quit. The framing
--   matches the quit-confirmation modal so the key hint lines up.
drawVictoryModal :: Widget ()
drawVictoryModal =
  centerLayer
    $ borderWithLabel (str " Victory! ")
    $ padAll 1
    $ vBox
        [ str "The dragon is slain and the dungeon falls silent."
        , str ""
        , str "You have won the run."
        , str ""
        , str "  q / Esc : exit to the prompt"
        ]

-- | A reference sheet for every key binding, modal, and slash
--   command the game currently understands. Opened with @?@ and
--   closed with any key. Mirrors the organization of the three
--   input layers (normal, modal, prompt) so the player can find
--   what they want by context rather than by alphabetical order.
drawHelpModal :: Widget ()
drawHelpModal =
  let section title rows = (title ++ ":") : rows ++ [""]
      lines_ =
           section "Movement"
             [ "  arrow keys / hjkl    move 4-way"
             , "  y u b n              move diagonally"
             , "  .                    wait a turn"
             ]
        ++ section "World actions"
             [ "  g                    pick up item here"
             , "  >                    descend stairs"
             , "  <                    ascend stairs"
             ]
        ++ section "Modals"
             [ "  i                    inventory"
             , "  Q                    quest log"
             , "  ?                    this help screen"
             , "  Esc                  close any open modal"
             ]
        ++ section "Dialogue (NPCs)"
             [ "  bump an NPC          open dialogue"
             , "  a-z                  accept offered quest"
             , "  A-Z                  hand in a ready quest"
             , "  Esc                  close without choosing"
             ]
        ++ section "Quest log (Q)"
             [ "  a-z                  select an active quest"
             , "  x                    abandon the selected quest"
             , "  Esc / Q              close the log"
             ]
        ++ section "Inventory (i)"
             [ "  a-z                  use / equip the item"
             , "  Esc / i              close the bag"
             ]
        ++ section "Slash commands (wizard / debug)"
             [ "  /                    open the command prompt"
             , "  /reveal              light up the entire map"
             , "  /heal                full HP restore"
             , "  /kill-all            banish every monster on level"
             , "  /teleport X Y        jump to a tile"
             , "  /spawn KIND          spawn rat/goblin/orc next to you"
             , "  /xp N                grant N XP"
             , "  /descend  /ascend    force-move one floor"
             ]
        ++ section "Quitting"
             [ "  q / Esc              open quit confirmation"
             , "  y                    confirm and exit the run"
             , "  n / Esc / any        cancel and keep playing"
             ]
        ++ [ "(press any key to close)" ]
  in centerLayer $ borderWithLabel (str " Help ") $ padAll 1 $ vBox (map str lines_)
