module Game.Render
  ( drawGame
  , Name (..)
  , fogAttr
  , npcAttr
  , bossAttr
  , doorAttr
  , lockedDoorAttr
  , chestFullAttr
  , chestEmptyAttr
  , saveMenuCursorAttr
  , saveMenuEmptyAttr
  , launchCursorAttr
  , launchDisabledAttr
  , launchTitleAttr
  ) where

import Brick
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Center (centerLayer)
import Data.List (find)
import qualified Data.Set as Set
import Data.Set (Set)
import Linear (V2(..))

import Game.Core
import Game.Utils.List (safeIndex)
import Game.Logic.Chest (Chest(..), ChestState(..))
import qualified Game.Logic.Inventory as Inv
import Game.Logic.Progression (xpForNextLevel)
import Game.Logic.Quest
  ( Quest(..), QuestStatus(..), questDescription, questProgressLabel )
import Game.Save.Types (SaveMetadata(..))
import Game.Types

-- | Centered bordered modal with a one-tile pad. The first argument
--   wraps the bordered panel before centering (use 'id' for no
--   extra constraint, or e.g. @hLimit 60@ for a narrow panel).
modalFrame :: (Widget Name -> Widget Name) -> String -> Widget Name -> Widget Name
modalFrame wrap title body =
  centerLayer $ wrap $ borderWithLabel (str (" " ++ title ++ " ")) $ padAll 1 body

-- | Brick widget-name type for this app.
--
--   Brick identifies stateful widgets (viewports, editors, lists,
--   and any site that shows a cursor) by a user-chosen name type.
--   Most of our UI is stateless and only ever needs a single
--   "default" tag — 'NameDefault' — which covers the cursor
--   parked on the prompt line. 'HelpViewport' tags the scrollable
--   region inside the help modal so its scroll position can be
--   driven from 'handleHelpKey'.
data Name = NameDefault | HelpViewport
  deriving (Eq, Ord, Show)

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

-- | Attribute name used for doors (both open and closed). Main
--   wires this to a yellow foreground so doors stand out from
--   walls and floor — the glyphs alone (@'@ and @+@) are easy
--   to lose against a dungeon wall without a color cue.
doorAttr :: AttrName
doorAttr = attrName "door"

-- | Attribute name used for locked doors. Main wires this to a red
--   foreground so a locked door reads as hostile/impassable at a
--   glance, without needing a distinct glyph.
lockedDoorAttr :: AttrName
lockedDoorAttr = attrName "lockedDoor"

-- | Attribute name for a full chest — a chest that still has loot
--   the player can collect. Main wires this to a bright color so
--   full chests are visually distinct from looted ones without
--   changing the glyph.
chestFullAttr :: AttrName
chestFullAttr = attrName "chestFull"

-- | Attribute name for an empty chest cooling down toward a refill.
--   Main wires this to a dim color so the same @=@ glyph still
--   reads as "a chest, but not worth walking to right now".
chestEmptyAttr :: AttrName
chestEmptyAttr = attrName "chestEmpty"

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

-- | Top-level Brick draw function. The 'Bool' is the @--wizard@
--   runtime flag: it's forwarded into the help modal so the wizard
--   command section is hidden when cheats are disabled, and never
--   advertised to a clean-run player.
drawGame :: Bool -> Bool -> GameState -> [Widget Name]
drawGame wizardEnabled aiOn gs =
  let baseLayer = vBox
        [ drawGrid gs
        , drawPromptLine gs
        , drawStatus gs
        , drawQuests gs
        , drawMessages gs
        , str "? for help    q/Esc to quit"
        ]
      -- Room descriptions are now pushed into gsMessages instead of
      -- rendered as a floating panel (see applyAIResponse).
  in case gsLaunchMenu gs of
       -- Launch / title screen swallows the whole viewport so no
       -- gameplay artifacts leak through before the player picks
       -- New Game / Continue / Load / Quit.
       Just lm -> [drawLaunchMenu gs lm]
       Nothing -> case gsDialogue gs of
         Just i | Just npc <- safeIndex i (gsNPCs gs) ->
           [drawDialogueModal aiOn (gsQuests gs) i npc, baseLayer]
         _
           | gsVictory      gs  -> [drawVictoryModal gs, baseLayer]
           | gsConfirmQuit  gs  -> [drawQuitConfirmModal, baseLayer]
           | gsHelpOpen     gs  -> [drawHelpModal wizardEnabled, baseLayer]
           | Just sm <- gsSaveMenu gs -> [drawSaveMenuModal sm, baseLayer]
           | Just vm <- gsVolumeMixer gs ->
               [drawVolumeMixerModal gs vm, baseLayer]
           | gsQuestLogOpen gs  -> [drawQuestLogModal gs, baseLayer]
           | gsInventoryOpen gs -> [drawInventoryModal gs, baseLayer]
           | Just keyNm <- gsLockedDoorPrompt gs ->
               [drawLockedDoorModal keyNm, baseLayer]
           | otherwise          -> [baseLayer]

-- | The line between the map and the status bar. When the
--   slash-command prompt is closed it's a blank spacer that holds
--   the terminal cursor (some terminals ignore hide-cursor
--   escapes, so we keep it visually quiet). When the prompt is
--   open it shows @> buf@ with the cursor parked after the buffer.
drawPromptLine :: GameState -> Widget Name
drawPromptLine gs = case gsPrompt gs of
  Nothing  ->
    showCursor NameDefault (Location (0, 0)) $ str " "
  Just buf ->
    let line = "> " ++ buf
    in showCursor NameDefault (Location (length line, 0)) $ str line

-- | Render the dungeon as a player-centered scrolling viewport.
--
--   The widget is 'Greedy' in both dimensions so Brick gives it
--   whatever space is left after the fixed-height status / prompt /
--   message widgets below it claim theirs. At render time we open
--   the Brick context for the actual available pixel dimensions and
--   compute the camera's top-left corner so the player glyph sits
--   at the visual center. There is no camera state on 'GameState':
--   the camera is a pure function of 'gsPlayerPos' and terminal
--   size, so moving the player on frame N automatically scrolls the
--   map on frame N+1.
--
--   Tiles outside the level bounds render as blank space — this
--   produces the "edge of the world" look when the player walks
--   toward a wall near the map's perimeter, matching the
--   "always-centered" camera policy.
drawGrid :: GameState -> Widget Name
drawGrid gs = Widget Greedy Greedy $ do
  ctx <- getContext
  let viewW = availWidth  ctx
      viewH = availHeight ctx
      dl    = gsLevel   gs
      vis   = gsVisible gs
      exp_  = gsExplored gs
      V2 px py = gsPlayerPos gs
      -- Top-left of the camera window. Integer-divide biases the
      -- player one column/row off-center on even dimensions, which
      -- is fine and matches how every other roguelike does it.
      camX  = px - viewW `div` 2
      camY  = py - viewH `div` 2
  render $ vBox
    [ hBox
        [ drawCellAt gs dl vis exp_ (V2 x y)
        | x <- [camX .. camX + viewW - 1] ]
    | y <- [camY .. camY + viewH - 1] ]

-- | Like 'drawCell', but returns a blank cell when the requested
--   position is outside the level's tile grid. Used by 'drawGrid'
--   to paint the void outside the dungeon once the camera window
--   overlaps the map boundary.
drawCellAt
  :: GameState -> DungeonLevel -> Set Pos -> Set Pos -> Pos -> Widget Name
drawCellAt gs dl vis exp_ pos@(V2 x y)
  | x < 0 || y < 0 || x >= dlWidth dl || y >= dlHeight dl = str " "
  | otherwise = drawCell gs vis exp_ pos

drawCell :: GameState -> Set Pos -> Set Pos -> Pos -> Widget Name
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
visibleCell :: GameState -> Pos -> Widget Name
visibleCell gs pos
  | pos == gsPlayerPos gs = str "@"
  | Just m <- find (`monsterOccupies` pos) (gsMonsters gs) =
      let attr = if isBoss (mKind m) then bossAttr else mempty
      in withAttr attr $ str [monsterGlyph (mKind m)]
  | Just _ <- find (\n -> npcPos n == pos) (gsNPCs gs) =
      withAttr npcAttr $ str "N"
  | Just (_, it) <- find (\(p, _) -> p == pos) (gsItemsOnFloor gs) =
      str [itemGlyph it]
  | Just c <- find (\ch -> chestPos ch == pos) (gsChests gs) =
      case chestState c of
        ChestFull _  -> withAttr chestFullAttr  $ str "="
        ChestEmpty _ -> withAttr chestEmptyAttr $ str "="
  | otherwise =
      let glyph = tileGlyph (gsLevel gs) pos
          -- Doors get their own color so @'@ and @+@ don't get
          -- lost against the floor/wall glyphs. Locked doors use
          -- a distinct attr so the player can tell at a glance
          -- that bumping them will need a key.
          wrap = case tileAt (gsLevel gs) pos of
            Just (Door (Locked _)) -> withAttr lockedDoorAttr
            Just (Door _)          -> withAttr doorAttr
            _                      -> id
      in wrap (str [glyph])

-- | Glyph for the terrain at a position, without the player or
--   monsters overlaid. Used for both visible and fogged rendering.
tileGlyph :: DungeonLevel -> Pos -> Char
tileGlyph dl pos = case tileAt dl pos of
  Just Floor              -> '.'
  Just Wall               -> '#'
  Just (Door Open)        -> '\''
  Just (Door Closed)      -> '+'
  Just (Door (Locked _))  -> '+'
  Just StairsDown         -> '>'
  Just StairsUp           -> '<'
  Nothing                 -> ' '

drawStatus :: GameState -> Widget Name
drawStatus gs =
  let s       = gsPlayerStats gs
      effStats = Inv.effectiveStats s (gsInventory gs)
      dl      = gsLevel gs
      dashStr
        | gsDashCooldown gs <= 0 = "ready"
        | otherwise              = show (gsDashCooldown gs)
      -- Run-stats readout: prefer the frozen victory snapshot so
      -- the timer stops ticking once the dragon falls, otherwise
      -- show the live counter.
      turnsShown = case gsFinalTurns gs of
        Just t  -> t
        Nothing -> gsTurnsElapsed gs
      status  = "LVL "   ++ show (sLevel s)
             ++ "   XP: "  ++ show (sXP s) ++ "/" ++ show (xpForNextLevel (sLevel s))
             ++ "   HP: "  ++ show (sHP s) ++ "/" ++ show (sMaxHP s)
             ++ "   ATK: " ++ show (sAttack effStats)
             ++ "   DEF: " ++ show (sDefense effStats)
             ++ "   Depth: " ++ show (dlDepth dl)
             ++ "   Dash: " ++ dashStr
             ++ "   T: "   ++ show turnsShown
             ++ "   P: "   ++ show (gsPotionsUsed gs)
             ++ (if gsDead gs then "   *** YOU DIED ***" else "")
  in str status

drawMessages :: GameState -> Widget Name
drawMessages gs = vBox (map str (take 3 (gsMessages gs)))

-- | One-line quest panel: each active quest is shown as
--   @"Name p/target"@, separated by two spaces. If there are no
--   quests (shouldn't normally happen), renders an empty line so
--   the layout doesn't jump.
drawQuests :: GameState -> Widget Name
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
drawInventoryModal :: GameState -> Widget Name
drawInventoryModal gs =
  let inv    = gsInventory gs
      header =
        [ "Equipped:"
        , "  Weapon: " ++ maybe "(none)" (itemName . IWeapon) (invWeapon inv)
        , "  Armor:  " ++ maybe "(none)" (itemName . IArmor)  (invArmor  inv)
        , "  Arrows: " ++ show (invArrows inv)
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
  in modalFrame id "Inventory" body

-- | Centered modal showing an NPC's greeting, any quests the
--   player is ready to turn in here, and the list of quests this
--   NPC currently has to offer.
--
--   * Capital letters @A@..@Z@ hand in a ready quest — a @*@ next
--     to the entry means this NPC is the original giver and will
--     pay the full bounty; no @*@ means they'll pay half.
--   * Lowercase letters @a@..@z@ accept a new offer.
--   * @Esc@ closes without any choice, leaving both lists intact.
drawDialogueModal :: Bool -> [Quest] -> Int -> NPC -> Widget Name
drawDialogueModal aiEnabled quests npcIdx npc =
  let ready = [ q | q <- quests, qStatus q == QuestReadyToTurnIn ]
      -- Prefer the AI-generated greeting when we have one cached;
      -- fall back to the hardcoded line otherwise. When AI is enabled
      -- but the greeting hasn't arrived yet, show a loading indicator
      -- so the player knows something is coming.
      greetingLine = case npcAIGreet npc of
        Just g  -> g
        Nothing | aiEnabled -> "..."
                | otherwise -> npcGreeting npc
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
  in modalFrame id (npcName npc) body
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
drawQuestLogModal :: GameState -> Widget Name
drawQuestLogModal gs =
  let qs         = gsQuests gs
      active     = [ q | q <- qs, qStatus q == QuestActive ]
      completed  = [ q | q <- qs, qStatus q == QuestCompleted ]
      failed     = [ q | q <- qs, qStatus q == QuestFailed ]
      cursor     = gsQuestLogCursor gs

      section title items fmt =
        let rows = case items of
              [] -> ["  (none)"]
              xs -> zipWith (curry fmt) [(0 :: Int) ..] xs
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
  in modalFrame id "Quest Log" body

-- | A tiny confirmation modal shown when the player presses @q@
--   or @Esc@ in normal mode. Prevents fat-fingered quits given
--   that @q@ (Quit) and @Q@ (Quest Log) are one shift-key apart.
drawQuitConfirmModal :: Widget Name
drawQuitConfirmModal =
  modalFrame id "Quit?"
    $ vBox
        [ str "Really quit this run?"
        , str ""
        , str "  y : yes, quit"
        , str "  n / Esc : keep playing"
        ]

-- | Centered modal with a music and an SFX volume slider. The
--   cursor highlights whichever channel is currently focused;
--   h / l (or Left / Right) nudge the focused slider in 5%
--   increments, 0..9 snap it to 0 %..90 %, and j / k (or Up /
--   Down, Tab) swap focus between the two. The slider is a
--   20-cell bar (each cell = 5 %) so the step size aligns
--   with the visual granularity.
drawVolumeMixerModal :: GameState -> VolumeMixer -> Widget Name
drawVolumeMixerModal gs vm =
  let cursor = vmCursor vm
      musicRow = renderRow "Music" (cursor == VolMusic) (gsMusicVolume gs)
      sfxRow   = renderRow "SFX  " (cursor == VolSfx)   (gsSfxVolume gs)
      body = vBox
        [ str "Adjust master volume for music and sound effects."
        , str ""
        , musicRow
        , sfxRow
        , str ""
        , str "  h / l or </>     softer / louder  (5%)"
        , str "  j / k or Tab     switch channel"
        , str "  0..9             jump to  0% .. 90%"
        , str "  m                mute focused channel"
        , str "  Esc              close"
        ]
  in modalFrame (hLimit 56) "Volume" body
  where
    renderRow label focused v =
      let marker  = if focused then " > " else "   "
          bar     = volumeBar v
          percent = volumePercent v
          line    = marker ++ label ++ "  [" ++ bar ++ "] " ++ percent
      in str line

    -- 20-cell bar where each cell represents 5% of the full range.
    -- The filled and empty cells use glyphs that render well on a
    -- bare terminal without ever needing truecolor or extra attrs.
    volumeBar v =
      let cells = 20 :: Int
          filled = max 0 . min cells $ round (v * fromIntegral cells :: Double)
      in replicate filled '#' ++ replicate (cells - filled) '-'

    volumePercent v =
      let pct = max 0 . min 100 $ round (v * 100 :: Double) :: Int
          padded
            | pct < 10  = "  " ++ show pct
            | pct < 100 = " "  ++ show pct
            | otherwise = show pct
      in padded ++ "%"

-- | Modal shown when the player bumps a locked door without the
--   matching key. The name is the already-formatted key name
--   (e.g. "brass key"); the modal exists purely to tell the
--   player /which/ key they need, and any keystroke dismisses it
--   via 'handleLockedDoorKey'.
drawLockedDoorModal :: String -> Widget Name
drawLockedDoorModal keyNm =
  modalFrame id "Locked"
    $ vBox
        [ str ("This door is locked. It needs the " ++ keyNm ++ ".")
        , str ""
        , str "  (press any key to continue)"
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
drawSaveMenuModal :: SaveMenu -> Widget Name
drawSaveMenuModal sm =
  let title = case smMode sm of
        SaveMode -> "Save Game"
        LoadMode -> "Load Game"
      rows = zipWith (drawRow (smMode sm) (smCursor sm)) [0 ..] (smSlots sm)
      hint = case (smMode sm, smConfirm sm) of
        (_, True) ->
          str "Overwrite this slot?   y : yes   n / Esc : cancel"
        (SaveMode, False) ->
          str "[a-z] select slot   x delete   Esc close"
        (LoadMode, False) ->
          str "[a-z] load   x delete   Esc close"
      body = vBox $ rows ++ [str "", hint]
  in modalFrame id title body
  where
    drawRow :: SaveMenuMode -> Int -> Int -> SaveMenuEntry -> Widget Name
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
drawLaunchMenu :: GameState -> LaunchMenu -> Widget Name
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
--   true) so the only thing the player can do is quit. The body
--   shows the full run-stats score card: how many turns the boss
--   kill took, how many potions were burned, how many saves were
--   used, the final depth and player level, and a rank bucket
--   computed by 'runRank'. The framing matches the
--   quit-confirmation modal so the key hint lines up.
drawVictoryModal :: GameState -> Widget Name
drawVictoryModal gs =
  let stats   = gsPlayerStats gs
      dl      = gsLevel gs
      turns   = case gsFinalTurns gs of
                  Just t  -> t
                  Nothing -> gsTurnsElapsed gs  -- defensive: victory modal implies Just
      statLine label value =
        str (padLabel label ++ show value)
      padLabel l = l ++ replicate (14 - length l) ' '
  in modalFrame id "Victory!"
       $ vBox
           [ str "The dragon is slain and the dungeon falls silent."
           , str ""
           , str "You have won the run."
           , str ""
           , statLine "Turns:"        turns
           , statLine "Potions used:" (gsPotionsUsed gs)
           , statLine "Saves used:"   (gsSavesUsed gs)
           , statLine "Final depth:"  (dlDepth dl)
           , statLine "Player level:" (sLevel stats)
           , str ""
           , str ("Rank: " ++ runRank gs)
           , str ""
           , str "  q / Esc : exit to the prompt"
           ]

-- | A reference sheet for every key binding, modal, and slash
--   command the game currently understands. Opened with @?@, scrolled
--   with arrow keys / @j@ / @k@ / PgUp / PgDn / Home / End, and
--   closed with @Esc@ or @?@.
--
--   The modal is sized as a percentage of the terminal and the body
--   sits inside a 'viewport' so it stays usable on terminals that
--   aren't tall enough to show every line at once. When cheats are
--   disabled the "Wizard commands" section is dropped entirely so a
--   clean-run player is never told those commands exist.
drawHelpModal :: Bool -> Widget Name
drawHelpModal wizardEnabled =
  let section title rows = (title ++ ":") : rows ++ [""]
      wizardSection
        | wizardEnabled =
            section "Wizard commands (--wizard enabled)"
              [ "  /reveal              light up the entire map"
              , "  /heal                full HP restore"
              , "  /kill-all            banish every monster on level"
              , "  /teleport X Y        jump to a tile"
              , "  /spawn KIND          spawn rat/goblin/orc next to you"
              , "  /xp N                grant N XP"
              , "  /descend  /ascend    force-move one floor"
              , "  (any save written after one of these is flagged"
              , "   and is invisible to non-wizard sessions)"
              ]
        | otherwise = []
      lines_ =
           section "Movement"
             [ "  arrow keys / hjkl    move 4-way"
             , "  y u b n              move diagonally"
             , "  Shift + arrow        dash up to 5 tiles (60-turn"
             , "                       cooldown; stops on any"
             , "                       obstacle, item, or stairs)"
             , "  HJKL YUBN            dash, vi-keys variant"
             , "  .                    wait a turn"
             ]
        ++ section "World actions"
             [ "  g                    pick up item here"
             , "  >                    descend stairs"
             , "  <                    ascend stairs"
             , "  f                    fire a bow (then pick a"
             , "                       direction; needs bow equipped"
             , "                       and at least one arrow)"
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
        ++ section "Slash commands"
             [ "  /                    open the command prompt"
             , "  /help                this help screen"
             , "  /quit                open quit confirmation"
             , "  /inv  /inventory     open the bag"
             , "  /quests /questlog    open the quest log"
             , "  /wait  /rest         pass a turn"
             , "  /save  /load         open save / load picker"
             , "  /quicksave  /qs      write the quicksave slot"
             , "  /quickload  /ql      read the quicksave slot"
             , "  /volume /vol /mixer  open the volume mixer"
             ]
        ++ section "Volume mixer (/volume)"
             [ "  h / l                softer / louder (5%)"
             , "  j / k / Tab          switch channel"
             , "  0-9                  snap to 0% .. 90%"
             , "  m                    mute focused channel"
             , "  Esc                  close"
             ]
        ++ wizardSection
        ++ section "Quitting"
             [ "  q / Esc              open quit confirmation"
             , "  y                    confirm and exit the run"
             , "  n / Esc / any        cancel and keep playing"
             ]
      body = viewport HelpViewport Vertical $ vBox (map str lines_)
      footer = str "↑/↓ jk scroll   PgUp/PgDn page   Home/End   Esc close"
  in centerLayer
       $ hLimit 64
       $ vLimitPercent 85
       $ borderWithLabel (str " Help ")
       $ padLeftRight 1
       $ vBox
           [ body
           , hBorder
           , footer
           ]
