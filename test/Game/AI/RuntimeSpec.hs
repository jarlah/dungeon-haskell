{-# LANGUAGE ScopedTypeVariables #-}
-- | Tests for the pure state-folding helpers in
--   "Game.AI.Runtime" — the 'GameState -> GameState' functions that
--   used to be trapped in @app/Main.hs@ and therefore untestable.
--
--   The IO-heavy request firing + response handling in
--   "Game.AI.Runtime" still lives in 'EventM' and is not exercised
--   here — that needs a fake 'Game.AI.Async.AIWorker' to test well,
--   which is a later milestone.
module Game.AI.RuntimeSpec (spec) where

import qualified Data.Set as Set
import qualified Data.Vector as V
import Linear (V2 (..))
import System.Random (mkStdGen)
import Test.Hspec

import Game.AI.Runtime (appendQuestToFirstNPC, describeNPCRole, updateNPCGreet)
import Game.Core
import Game.Logic.Quest (Quest (..), QuestGoal (..), mkQuest)
import Game.Types

-- | A minimal 5x5 level used only as a place to anchor the fixture.
--   None of the tests in this module care about the layout.
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

-- | Build an NPC with no AI greeting cached and a given name +
--   offer list.
mkNPC :: String -> [Quest] -> NPC
mkNPC name offers = NPC
  { npcPos      = V2 1 1
  , npcName     = name
  , npcGreeting = "Hail."
  , npcAIGreet  = Nothing
  , npcOffers   = offers
  }

-- | A tiny baseline 'GameState' with a configurable NPC list. All
--   the fields the tests don't care about get sensible defaults.
mkFixture :: [NPC] -> GameState
mkFixture npcs = GameState
  { gsLevel       = tinyRoom
  , gsPlayerPos   = V2 1 1
  , gsPlayerStats = Stats
      { sHP = 20, sMaxHP = 20
      , sAttack = 5, sDefense = 2, sSpeed = 10
      , sLevel = 1, sXP = 0
      }
  , gsMonsters      = []
  , gsMessages      = []
  , gsRng           = mkStdGen 0
  , gsDead          = False
  , gsQuitting      = False
  , gsEvents        = []
  , gsVisible       = Set.empty
  , gsExplored      = Set.empty
  , gsPrompt        = Nothing
  , gsInventory     = emptyInventory
  , gsItemsOnFloor  = []
  , gsInventoryOpen = False
  , gsQuests        = []
  , gsNPCs          = npcs
  , gsDialogue      = Nothing
  , gsQuestLogOpen   = False
  , gsQuestLogCursor = Nothing
  , gsConfirmQuit    = False
  , gsHelpOpen       = False
  , gsBossDepth      = 10
  , gsBossRoom       = Nothing
  , gsVictory        = False
  , gsLevels         = mempty
  , gsSaveMenu       = Nothing
  , gsLaunchMenu     = Nothing
  , gsRoomDesc       = Nothing
  , gsRoomDescVisible = False
  , gsAwaitingDirection = Nothing
  , gsCheatsUsed     = False
  , gsNextKeyId      = 0
  , gsPendingKeys    = []
  , gsLockedDoorPrompt = Nothing
  , gsDashCooldown   = 0
  , gsRegenCounter   = 0
  , gsTurnsElapsed   = 0
  , gsPotionsUsed    = 0
  , gsSavesUsed      = 0
  , gsFinalTurns     = Nothing
  , gsChests         = []
  , gsMusicVolume    = 0.7
  , gsSfxVolume      = 1.0
  , gsVolumeMixer    = Nothing
  }

-- | A small canned quest used by the 'appendQuestToFirstNPC' tests.
sampleQuest :: Quest
sampleQuest = mkQuest "Test Quest" (GoalKillMonsters 3)

spec :: Spec
spec = do

  describe "describeNPCRole" $
    it "returns 'Quest Master' for any NPC (MVP stub)" $ do
      describeNPCRole (mkNPC "Aldo"  []) `shouldBe` "Quest Master"
      describeNPCRole (mkNPC "Bella" []) `shouldBe` "Quest Master"

  describe "updateNPCGreet" $ do

    it "stamps the greeting into the NPC at the requested index" $ do
      let gs  = mkFixture [mkNPC "Aldo" [], mkNPC "Bella" []]
          gs' = updateNPCGreet 1 "Good day, traveller." gs
      map npcAIGreet (gsNPCs gs')
        `shouldBe` [Nothing, Just "Good day, traveller."]

    it "is a no-op when the index is out of bounds (too large)" $ do
      let gs  = mkFixture [mkNPC "Aldo" []]
          gs' = updateNPCGreet 5 "ignored" gs
      map npcAIGreet (gsNPCs gs') `shouldBe` [Nothing]

    it "is a no-op when the index is negative" $ do
      let gs  = mkFixture [mkNPC "Aldo" []]
          gs' = updateNPCGreet (-1) "ignored" gs
      map npcAIGreet (gsNPCs gs') `shouldBe` [Nothing]

    it "is a no-op when gsNPCs is empty" $ do
      let gs  = mkFixture []
          gs' = updateNPCGreet 0 "ignored" gs
      gsNPCs gs' `shouldBe` []

    it "overwrites an existing cached greeting" $ do
      let seeded =
            (mkNPC "Aldo" []) { npcAIGreet = Just "old" }
          gs  = mkFixture [seeded]
          gs' = updateNPCGreet 0 "new" gs
      map npcAIGreet (gsNPCs gs') `shouldBe` [Just "new"]

    it "leaves every NPC except the targeted one untouched" $ do
      let gs  = mkFixture
            [ mkNPC "Aldo"    []
            , (mkNPC "Bella"  []) { npcAIGreet = Just "kept" }
            , mkNPC "Cato"    []
            ]
          gs' = updateNPCGreet 0 "stamped" gs
      map npcAIGreet (gsNPCs gs')
        `shouldBe` [Just "stamped", Just "kept", Nothing]

  describe "appendQuestToFirstNPC" $ do

    it "appends the quest to the first NPC's offer list" $ do
      let gs  = mkFixture [mkNPC "Aldo" [], mkNPC "Bella" []]
          gs' = appendQuestToFirstNPC sampleQuest gs
      case gsNPCs gs' of
        (a : _) -> map qName (npcOffers a) `shouldBe` ["Test Quest"]
        []      -> expectationFailure "lost the NPC list"

    it "leaves the non-first NPCs untouched" $ do
      let existing = mkQuest "Existing" (GoalKillMonsters 1)
          gs  = mkFixture
            [ mkNPC "Aldo"  []
            , mkNPC "Bella" [existing]
            ]
          gs' = appendQuestToFirstNPC sampleQuest gs
      case gsNPCs gs' of
        [_, b] -> map qName (npcOffers b) `shouldBe` ["Existing"]
        _      -> expectationFailure "NPC list reshape unexpected"

    it "preserves order: old offers first, new quest last" $ do
      let firstOffer = mkQuest "Ancient" (GoalKillMonsters 5)
          gs  = mkFixture [mkNPC "Aldo" [firstOffer]]
          gs' = appendQuestToFirstNPC sampleQuest gs
      case gsNPCs gs' of
        (a : _) -> map qName (npcOffers a) `shouldBe` ["Ancient", "Test Quest"]
        []      -> expectationFailure "lost the NPC list"

    it "silently drops the quest when gsNPCs is empty" $ do
      let gs  = mkFixture []
          gs' = appendQuestToFirstNPC sampleQuest gs
      gsNPCs gs' `shouldBe` []
