{-# LANGUAGE ScopedTypeVariables #-}
-- | Tests for the semantic-event list that 'applyAction' emits.
--
--   These are deterministic: we construct tiny fixture 'GameState's
--   with hand-picked RNG + stats so the combat outcome is forced.
module Game.GameStateSpec (spec) where

import qualified Data.Set as Set
import qualified Data.Vector as V
import Linear (V2(..))
import System.Random (mkStdGen)
import Test.Hspec

import Game.GameState
import Game.Logic.Command (Command(..))
import Game.Logic.Quest
  ( Quest(..), QuestGoal(..), QuestStatus(..), mkQuest )
import Game.Save (decodeSave, encodeSave)
import Game.Types

-- | A tiny 5x5 open room with walls around the edge, so the player
--   has somewhere to move and monsters have space to act.
tinyRoom :: DungeonLevel
tinyRoom = DungeonLevel
  { dlWidth  = 5
  , dlHeight = 5
  , dlDepth  = 1
  , dlTiles  = V.generate (5 * 5) $ \i ->
      let (y, x) = i `divMod` 5
      in if x == 0 || y == 0 || x == 4 || y == 4 then Wall else Floor
  , dlRooms  = [Room 1 1 3 3]
  }

-- | Build a 'GameState' with the player at a chosen spot, given
--   stats, and a known RNG seed.
mkFixture :: Int -> Pos -> Stats -> [Monster] -> GameState
mkFixture seed ppos pstats monsters = GameState
  { gsLevel       = tinyRoom
  , gsPlayerPos   = ppos
  , gsPlayerStats = pstats
  , gsMonsters    = monsters
  , gsMessages    = []
  , gsRng         = mkStdGen seed
  , gsDead        = False
  , gsQuitting    = False
  , gsEvents      = [EvAttackHit]  -- stale, should be cleared by applyAction
  , gsVisible       = Set.empty     -- applyAction recomputes these
  , gsExplored      = Set.empty
  , gsPrompt        = Nothing
  , gsInventory     = emptyInventory
  , gsItemsOnFloor  = []
  , gsInventoryOpen = False
  , gsQuests        = []
  , gsNPCs          = []
  , gsDialogue      = Nothing
  , gsQuestLogOpen   = False
  , gsQuestLogCursor = Nothing
  , gsConfirmQuit    = False
  , gsHelpOpen       = False
  , gsBossDepth      = 10
  , gsBossRoom       = Nothing
  , gsVictory        = False
  , gsLevels        = mempty
  , gsSaveMenu       = Nothing
  , gsLaunchMenu     = Nothing
  , gsRoomDesc        = Nothing
  , gsRoomDescVisible = False
  }

-- | Player stats strong enough to one-shot anything normal.
overpoweredPlayer :: Stats
overpoweredPlayer = Stats
  { sHP = 20, sMaxHP = 20
  , sAttack = 9999, sDefense = 99
  , sSpeed = 10
  , sLevel = 1, sXP = 0
  }

-- | A rat placed at (2, 1), one tile north of a player at (2, 2).
ratAt :: Pos -> Monster
ratAt = mkMonster Rat

spec :: Spec
spec = describe "Game.GameState.applyAction / event emission" $ do

  it "a blocked move produces no events and clears any stale ones" $ do
    -- Player at (1,1); walking N would hit the wall at (1,0).
    let gs  = mkFixture 42 (V2 1 1) overpoweredPlayer []
        gs' = applyAction (Move N) gs
    gsEvents gs' `shouldBe` []

  it "Wait on an empty level produces no events" $ do
    let gs  = mkFixture 1 (V2 2 2) overpoweredPlayer []
        gs' = applyAction Wait gs
    gsEvents gs' `shouldBe` []

  it "an unobstructed move produces no events" $ do
    -- Player at (2,2), moving E into an empty floor tile.
    let gs  = mkFixture 7 (V2 2 2) overpoweredPlayer []
        gs' = applyAction (Move E) gs
    gsEvents gs' `shouldBe` []

  it "killing a rat emits EvMonsterKilled" $ do
    -- Player at (2,2), rat at (2,1). Move N bumps the rat. Seed 3 is
    -- known to roll a Kill against a rat for this stat block (verified
    -- in GHCi — hit threshold bottoms at 5 so we can't assume any seed
    -- works; we pick one that does).
    let rat = ratAt (V2 2 1)
        gs  = mkFixture 3 (V2 2 2) overpoweredPlayer [rat]
        gs' = applyAction (Move N) gs
    EvMonsterKilled `elem` gsEvents gs' `shouldBe` True

  it "the rat is removed from the level after the killing blow" $ do
    let rat = ratAt (V2 2 1)
        gs  = mkFixture 3 (V2 2 2) overpoweredPlayer [rat]
        gs' = applyAction (Move N) gs
    length (gsMonsters gs') `shouldBe` 0

  it "killing a monster can fire EvLevelUp when enough XP is gained" $ do
    -- Put the player one XP away from level 2 and kill a rat (5 XP).
    let almostLeveled = overpoweredPlayer { sXP = 24 }   -- curve: 25 -> 2
        rat           = ratAt (V2 2 1)
        gs            = mkFixture 3 (V2 2 2) almostLeveled [rat]
        gs'           = applyAction (Move N) gs
    EvLevelUp `elem` gsEvents gs' `shouldBe` True

  it "events are fresh each turn (stale events don't leak)" $ do
    -- The fixture seeds a stale EvAttackHit; an empty-tile move must
    -- not preserve it.
    let gs  = mkFixture 99 (V2 2 2) overpoweredPlayer []
        gs' = applyAction Wait gs
    EvAttackHit `elem` gsEvents gs' `shouldBe` False

  -- ----------------------------------------------------------------
  -- M10: NPCs and dialogue
  -- ----------------------------------------------------------------

  it "bumping into an NPC opens dialogue and does not advance monsters" $ do
    -- Player at (2,2); NPC at (2,1). Move N bumps the NPC.
    -- A rat is placed non-adjacently so any monster activity would
    -- be visible via gsMonsters changes (rat can't reach in one turn).
    let npc = mkQuestMaster (V2 2 1)
        rat = ratAt (V2 3 3)
        gs  = (mkFixture 1 (V2 2 2) overpoweredPlayer [rat])
                { gsNPCs = [npc] }
        gs' = applyAction (Move N) gs
    gsDialogue gs'  `shouldBe` Just 0
    -- Monsters should NOT have acted — the event log is empty of
    -- player-hurt events, and the rat is still at (3,3).
    map mPos (gsMonsters gs') `shouldBe` [V2 3 3]

  it "accepting a quest moves it from the NPC into gsQuests" $ do
    let npc = mkQuestMaster (V2 2 1)
        gs  = (mkFixture 1 (V2 2 2) overpoweredPlayer [])
                { gsNPCs = [npc], gsDialogue = Just 0 }
        gs' = acceptQuestFromNPC 0 0 gs
    -- The offer list on the NPC shrinks by one.
    length (npcOffers (head (gsNPCs gs'))) `shouldBe`
      length (npcOffers npc) - 1
    -- And the accepted quest lands in gsQuests with QuestActive status.
    length (gsQuests gs') `shouldBe` 1
    qStatus (head (gsQuests gs')) `shouldBe` QuestActive

  it "a killed monster advances an accepted NPC quest to ready-to-turn-in" $ do
    -- Accept a kill-1-monster quest, then kill a rat adjacent to
    -- the player. Quest should flip to ready-to-turn-in (it still
    -- needs to be handed back to an NPC for the reward).
    let offer       = (mkQuest "Tiny Slayer" (GoalKillMonsters 1))
                        { qStatus = QuestNotStarted }
        npc         = (mkQuestMaster (V2 0 0)) { npcOffers = [offer] }
        rat         = ratAt (V2 2 1)
        gs0         = (mkFixture 3 (V2 2 2) overpoweredPlayer [rat])
                        { gsNPCs = [npc] }
        gs1         = acceptQuestFromNPC 0 0 gs0
        gs2         = applyAction (Move N) gs1     -- kill the rat
    length (gsQuests gs2) `shouldBe` 1
    qStatus (head (gsQuests gs2)) `shouldBe` QuestReadyToTurnIn

  -- ----------------------------------------------------------------
  -- M10.2: quest log / abandon
  -- ----------------------------------------------------------------

  it "abandonQuest flips the selected active quest to QuestFailed" $ do
    let slayer = mkQuest "Slayer" (GoalKillMonsters 5)
        delve  = mkQuest "Delve"  (GoalReachDepth 3)
        gs     = (mkFixture 1 (V2 2 2) overpoweredPlayer [])
                   { gsQuests = [slayer, delve] }
        gs'    = abandonQuest 0 gs  -- abandon Slayer
    map qStatus (gsQuests gs') `shouldBe` [QuestFailed, QuestActive]

  it "abandonQuest is a no-op on an out-of-range index" $ do
    let slayer = mkQuest "Slayer" (GoalKillMonsters 5)
        gs     = (mkFixture 1 (V2 2 2) overpoweredPlayer [])
                   { gsQuests = [slayer] }
        gs'    = abandonQuest 5 gs
    gsQuests gs' `shouldBe` gsQuests gs

  it "abandonQuest skips past non-active quests when indexing" $ do
    -- A mixed bag: completed, active, active. Abandoning index 1
    -- of the *active* sub-list should target the THIRD underlying
    -- quest (not the first active, which is index 0).
    let done   = (mkQuest "Done"  (GoalKillMonsters 1)) { qStatus = QuestCompleted }
        a1     = mkQuest "A1" (GoalKillMonsters 2)
        a2     = mkQuest "A2" (GoalKillMonsters 3)
        gs     = (mkFixture 1 (V2 2 2) overpoweredPlayer [])
                   { gsQuests = [done, a1, a2] }
        gs'    = abandonQuest 1 gs
    map qStatus (gsQuests gs')
      `shouldBe` [QuestCompleted, QuestActive, QuestFailed]

  it "abandoning a quest clears the quest log cursor" $ do
    let q  = mkQuest "T" (GoalKillMonsters 1)
        gs = (mkFixture 1 (V2 2 2) overpoweredPlayer [])
               { gsQuests         = [q]
               , gsQuestLogOpen   = True
               , gsQuestLogCursor = Just 0
               }
        gs' = abandonQuest 0 gs
    gsQuestLogCursor gs' `shouldBe` Nothing

  -- ----------------------------------------------------------------
  -- M12: quest turn-in and XP rewards
  -- ----------------------------------------------------------------

  it "goal-met quests flip to ready-to-turn-in without awarding XP" $ do
    -- Slayer with target 1 and a fat reward. Kill a rat — the
    -- quest should go ready but XP stays at its pre-kill value
    -- modulo the small XP the rat itself gives via P.gainXP.
    -- To isolate the quest reward we check only that XP is NOT
    -- yet inflated by the 100 reward.
    let offer = (mkQuest "TinySlayer" (GoalKillMonsters 1))
                  { qStatus = QuestNotStarted, qReward = 100 }
        npc   = (mkQuestMaster (V2 0 0)) { npcOffers = [offer] }
        rat   = ratAt (V2 2 1)
        gs0   = (mkFixture 3 (V2 2 2) overpoweredPlayer [rat])
                  { gsNPCs = [npc] }
        gs1   = acceptQuestFromNPC 0 0 gs0
        gs2   = applyAction (Move N) gs1
    qStatus (head (gsQuests gs2)) `shouldBe` QuestReadyToTurnIn
    -- 5 XP from the rat kill itself, no 100-XP reward yet.
    sXP (gsPlayerStats gs2) `shouldBe` 5

  it "turning in at the original giver awards full XP and fires EvQuestTurnedIn" $ do
    let offer = (mkQuest "TinySlayer" (GoalKillMonsters 1))
                  { qStatus = QuestNotStarted, qReward = 10 }
        npc   = (mkQuestMaster (V2 0 0)) { npcOffers = [offer] }
        rat   = ratAt (V2 2 1)
        gs0   = (mkFixture 3 (V2 2 2) overpoweredPlayer [rat])
                  { gsNPCs = [npc] }
        gs1   = acceptQuestFromNPC 0 0 gs0
        gs2   = applyAction (Move N) gs1          -- kill, goes ready
        xpPre = sXP (gsPlayerStats gs2)
        gs3   = turnInQuest 0 0 gs2               -- hand in at original NPC
    qStatus (head (gsQuests gs3)) `shouldBe` QuestCompleted
    sXP (gsPlayerStats gs3) `shouldBe` xpPre + 10
    EvQuestTurnedIn `elem` gsEvents gs3 `shouldBe` True

  it "turning in at a non-giver awards half XP" $ do
    -- Two NPCs. The Quest Master at index 0 gives the quest;
    -- a "Stranger" at index 1 accepts the turn-in for half.
    let offer    = (mkQuest "TinySlayer" (GoalKillMonsters 1))
                     { qStatus = QuestNotStarted, qReward = 20 }
        giver    = (mkQuestMaster (V2 0 0)) { npcOffers = [offer] }
        stranger = NPC
          { npcName     = "Stranger"
          , npcPos      = V2 3 3
          , npcGreeting = "Well met."
          , npcAIGreet  = Nothing
          , npcOffers   = []
          }
        rat      = ratAt (V2 2 1)
        gs0      = (mkFixture 3 (V2 2 2) overpoweredPlayer [rat])
                     { gsNPCs = [giver, stranger] }
        gs1      = acceptQuestFromNPC 0 0 gs0       -- giver stamps qGiver=Just 0
        gs2      = applyAction (Move N) gs1         -- kill, goes ready
        xpPre    = sXP (gsPlayerStats gs2)
        gs3      = turnInQuest 1 0 gs2              -- hand in at the Stranger
    qStatus (head (gsQuests gs3)) `shouldBe` QuestCompleted
    sXP (gsPlayerStats gs3) `shouldBe` xpPre + (20 `div` 2)

  it "turn-in can trigger a level up and fire EvLevelUp" $ do
    -- Put the player near the level threshold and give the quest
    -- a big enough reward to cross it. Level curve at L1 = 25 XP.
    let almost = overpoweredPlayer { sXP = 0 }
        offer  = (mkQuest "BigBounty" (GoalKillMonsters 1))
                   { qStatus = QuestNotStarted, qReward = 30 }
        npc    = (mkQuestMaster (V2 0 0)) { npcOffers = [offer] }
        rat    = ratAt (V2 2 1)
        gs0    = (mkFixture 3 (V2 2 2) almost [rat]) { gsNPCs = [npc] }
        gs1    = acceptQuestFromNPC 0 0 gs0
        gs2    = applyAction (Move N) gs1             -- 5 XP from rat
        gs3    = turnInQuest 0 0 gs2                  -- +30 XP pushes past 25
    sLevel (gsPlayerStats gs3) `shouldBe` 2
    EvLevelUp       `elem` gsEvents gs3 `shouldBe` True
    EvQuestTurnedIn `elem` gsEvents gs3 `shouldBe` True

  it "turnInQuest is a no-op on a non-ready quest" $ do
    let q  = mkQuest "Active" (GoalKillMonsters 5)  -- still active
        gs = (mkFixture 1 (V2 2 2) overpoweredPlayer [])
               { gsQuests = [q]
               , gsNPCs   = [mkQuestMaster (V2 0 0)]
               }
        gs' = turnInQuest 0 0 gs
    gsQuests gs' `shouldBe` gsQuests gs
    sXP (gsPlayerStats gs') `shouldBe` sXP (gsPlayerStats gs)

  -- ----------------------------------------------------------------
  -- M11: boss encounter
  -- ----------------------------------------------------------------

  it "attacking any tile of a dragon's 2x2 footprint hits the boss" $ do
    -- Dragon occupies (1,1), (2,1), (1,2), (2,2). Player at (3,2)
    -- moves W into (2,2), which is the dragon's bottom-right tile.
    -- 'monsterAt' should resolve that to the dragon.
    let dragon = mkMonster Dragon (V2 1 1)
        gs0    = mkFixture 3 (V2 3 2) overpoweredPlayer [dragon]
        gs1    = applyAction (Move W) gs0
    -- overpoweredPlayer one-shots the dragon (attack 9999 vs HP 80).
    length (gsMonsters gs1) `shouldBe` 0
    EvBossKilled `elem` gsEvents gs1 `shouldBe` True
    gsVictory gs1 `shouldBe` True

  it "killing the dragon advances an accepted GoalKillBoss quest to ready" $ do
    let offer  = (mkQuest "Slay the Dragon" GoalKillBoss)
                   { qStatus = QuestNotStarted, qReward = 500 }
        npc    = (mkQuestMaster (V2 0 0)) { npcOffers = [offer] }
        dragon = mkMonster Dragon (V2 1 1)
        gs0    = (mkFixture 3 (V2 3 2) overpoweredPlayer [dragon])
                   { gsNPCs = [npc] }
        gs1    = acceptQuestFromNPC 0 0 gs0
        gs2    = applyAction (Move W) gs1
    length (gsQuests gs2) `shouldBe` 1
    qStatus (head (gsQuests gs2)) `shouldBe` QuestReadyToTurnIn

  it "GoalKillBoss quest does NOT advance on a regular monster kill" $ do
    let offer = (mkQuest "Slay the Dragon" GoalKillBoss)
                  { qStatus = QuestNotStarted, qReward = 500 }
        npc   = (mkQuestMaster (V2 0 0)) { npcOffers = [offer] }
        rat   = ratAt (V2 2 1)
        gs0   = (mkFixture 3 (V2 2 2) overpoweredPlayer [rat])
                  { gsNPCs = [npc] }
        gs1   = acceptQuestFromNPC 0 0 gs0
        gs2   = applyAction (Move N) gs1
    -- Rat dies but the boss-kill quest is still active, not ready.
    qStatus (head (gsQuests gs2)) `shouldBe` QuestActive

  it "descending onto the boss depth generates a boss floor" $ do
    -- Force the next-floor generation into boss mode by setting
    -- gsBossDepth = 2 and using the wizard /descend to skip from
    -- the fixture's depth-1 tinyRoom to a generated depth-2 floor.
    let gs0 = (mkFixture 7 (V2 1 1) overpoweredPlayer [])
                { gsBossDepth = 2 }
        gs1 = applyCommand CmdDescend gs0
    dlDepth (gsLevel gs1) `shouldBe` 2
    -- Boss floor: no StairsDown anywhere in the tile grid.
    StairsDown `notElem` V.toList (dlTiles (gsLevel gs1)) `shouldBe` True
    -- And a dragon is in the monster list.
    any ((== Dragon) . mKind) (gsMonsters gs1) `shouldBe` True
    -- And gsBossRoom is populated so the music layer can react.
    case gsBossRoom gs1 of
      Just _  -> pure ()
      Nothing -> expectationFailure "expected gsBossRoom to be set on a boss floor"

  -- ----------------------------------------------------------------
  -- M13: save/load snapshot semantics
  --
  -- The save system serializes the whole 'GameState' as one blob,
  -- which means modal flags ride along with it. Verify that a
  -- save/load round-trip preserves those flags so the player comes
  -- back to exactly the UI state they left. The filesystem layer
  -- has its own tests in Game.SaveSpec — here we go through the
  -- pure encodeSave / decodeSave codec so the test stays hermetic.
  -- ----------------------------------------------------------------

  it "save/load preserves the inventory-open modal flag" $ do
    let gs0 = (mkFixture 1 (V2 2 2) overpoweredPlayer [])
                { gsInventoryOpen = True }
    case decodeSave (encodeSave gs0) of
      Right gs' -> gsInventoryOpen gs' `shouldBe` True
      Left e    ->
        expectationFailure ("decode failed: " ++ show e)

  it "save/load preserves the quest-log-open modal flag" $ do
    let gs0 = (mkFixture 1 (V2 2 2) overpoweredPlayer [])
                { gsQuestLogOpen   = True
                , gsQuestLogCursor = Just 2
                }
    case decodeSave (encodeSave gs0) of
      Right gs' -> do
        gsQuestLogOpen   gs' `shouldBe` True
        gsQuestLogCursor gs' `shouldBe` Just 2
      Left e -> expectationFailure ("decode failed: " ++ show e)

  it "save/load preserves player position and messages" $ do
    let gs0 = (mkFixture 1 (V2 3 3) overpoweredPlayer [])
                { gsMessages = ["hello", "world"] }
    case decodeSave (encodeSave gs0) of
      Right gs' -> do
        gsPlayerPos gs' `shouldBe` V2 3 3
        gsMessages  gs' `shouldBe` ["hello", "world"]
      Left e -> expectationFailure ("decode failed: " ++ show e)

-- | Build a test NPC with two starter offers, placed at the
--   given position.
mkQuestMaster :: Pos -> NPC
mkQuestMaster p = NPC
  { npcName     = "Quest Master"
  , npcPos      = p
  , npcGreeting = "Hello."
  , npcAIGreet  = Nothing
  , npcOffers   =
      [ (mkQuest "Slayer" (GoalKillMonsters 5)) { qStatus = QuestNotStarted }
      , (mkQuest "Delve"  (GoalReachDepth   3)) { qStatus = QuestNotStarted }
      ]
  }
