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
import Game.Logic.Quest
  ( Quest(..), QuestGoal(..), QuestStatus(..), mkQuest )
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
  , gsLevels        = mempty
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
ratAt p = Monster
  { mKind  = Rat
  , mPos   = p
  , mStats = monsterStats Rat
  }

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

  it "a killed monster advances an accepted NPC quest" $ do
    -- Accept a kill-1-monster quest, then kill a rat adjacent to
    -- the player. Quest should flip to completed.
    let offer       = (mkQuest "Tiny Slayer" (GoalKillMonsters 1))
                        { qStatus = QuestNotStarted }
        npc         = (mkQuestMaster (V2 0 0)) { npcOffers = [offer] }
        rat         = ratAt (V2 2 1)
        gs0         = (mkFixture 3 (V2 2 2) overpoweredPlayer [rat])
                        { gsNPCs = [npc] }
        gs1         = acceptQuestFromNPC 0 0 gs0
        gs2         = applyAction (Move N) gs1     -- kill the rat
    length (gsQuests gs2) `shouldBe` 1
    qStatus (head (gsQuests gs2)) `shouldBe` QuestCompleted

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

-- | Build a test NPC with two starter offers, placed at the
--   given position.
mkQuestMaster :: Pos -> NPC
mkQuestMaster p = NPC
  { npcName     = "Quest Master"
  , npcPos      = p
  , npcGreeting = "Hello."
  , npcOffers   =
      [ (mkQuest "Slayer" (GoalKillMonsters 5)) { qStatus = QuestNotStarted }
      , (mkQuest "Delve"  (GoalReachDepth   3)) { qStatus = QuestNotStarted }
      ]
  }
