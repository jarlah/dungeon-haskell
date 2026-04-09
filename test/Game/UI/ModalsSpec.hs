-- | Tests for the pure helpers in "Game.UI.Modals". The full
--   'EventM' handlers need Brick state and IO for audio, but the
--   four state-folding cores — auto-close, dialogue accept,
--   dialogue turn-in, and quest-log select — carry all the
--   branching logic and are straightforward to pin down.
module Game.UI.ModalsSpec (spec) where

import Linear (V2 (..))
import Test.Hspec

import Game.GameState
  ( GameState (..), NPC (..), hardcodedInitialState )
import Game.Logic.Quest
  ( Quest (..), QuestGoal (..), QuestStatus (..), mkQuest )
import Data.Maybe (isJust, isNothing)
import Game.UI.Modals
  ( stepAutoClose, stepDialogueAccept, stepDialogueTurnIn
  , stepQuestLogSelect
  )

-- | Build an NPC carrying a given list of offers. The scalar
--   fields are only read by the renderer, so defaults keep the
--   call sites short.
mkNPC :: String -> [Quest] -> NPC
mkNPC name offers = NPC
  { npcName     = name
  , npcPos      = V2 0 0
  , npcGreeting = "hi"
  , npcAIGreet  = Nothing
  , npcOffers   = offers
  }

-- | A bare quest used for offer lists. Status doesn't matter for
--   the accept test — 'acceptQuestFromNPC' rewrites it.
offer :: String -> Quest
offer name = mkQuest name (GoalKillMonsters 1)

-- | A quest in the 'QuestReadyToTurnIn' state, used for turn-in
--   tests.
readyQuest :: String -> Quest
readyQuest name = (mkQuest name (GoalKillMonsters 1))
  { qStatus = QuestReadyToTurnIn }

-- | A quest in the 'QuestActive' state, used for quest-log
--   selection tests.
activeQuest :: String -> Quest
activeQuest name = (mkQuest name (GoalKillMonsters 1))
  { qStatus = QuestActive }

base :: GameState
base = hardcodedInitialState

spec :: Spec
spec = do

  describe "stepAutoClose" $ do

    it "closes the dialogue when the NPC has no offers and no quests are ready" $ do
      let gs = base
            { gsNPCs     = [mkNPC "alice" []]
            , gsQuests   = []
            , gsDialogue = Just 0
            }
      gsDialogue (stepAutoClose 0 gs) `shouldBe` Nothing

    it "keeps the dialogue open while the NPC still has offers" $ do
      let gs = base
            { gsNPCs     = [mkNPC "alice" [offer "slay the rat"]]
            , gsQuests   = []
            , gsDialogue = Just 0
            }
      gsDialogue (stepAutoClose 0 gs) `shouldBe` Just 0

    it "keeps the dialogue open while the player has ready-to-turn-in quests" $ do
      let gs = base
            { gsNPCs     = [mkNPC "alice" []]
            , gsQuests   = [readyQuest "fetch the amulet"]
            , gsDialogue = Just 0
            }
      gsDialogue (stepAutoClose 0 gs) `shouldBe` Just 0

    it "closes the dialogue when the NPC index is out of range" $ do
      let gs = base
            { gsNPCs     = []
            , gsQuests   = []
            , gsDialogue = Just 0
            }
      gsDialogue (stepAutoClose 0 gs) `shouldBe` Nothing

  describe "stepDialogueAccept" $ do

    it "returns Nothing when the letter points past the offer list" $ do
      let gs = base { gsNPCs = [mkNPC "alice" [offer "one"]] }
      isNothing (stepDialogueAccept 0 'c' gs) `shouldBe` True

    it "returns Just when the letter points at a valid offer" $ do
      let gs = base { gsNPCs = [mkNPC "alice" [offer "one", offer "two"]]
                    , gsQuests = []
                    }
      case stepDialogueAccept 0 'a' gs of
        Just gs' -> length (gsQuests gs') `shouldBe` 1
        Nothing  -> expectationFailure "expected Just, got Nothing"

    it "removes the accepted offer from the NPC" $ do
      let gs = base { gsNPCs = [mkNPC "alice" [offer "one", offer "two"]]
                    , gsQuests = []
                    }
      case stepDialogueAccept 0 'a' gs of
        Just gs' -> length (npcOffers (head (gsNPCs gs'))) `shouldBe` 1
        Nothing  -> expectationFailure "expected Just, got Nothing"

    it "returns Nothing when the NPC index is out of range" $ do
      let gs = base { gsNPCs = [] }
      isNothing (stepDialogueAccept 0 'a' gs) `shouldBe` True

  describe "stepDialogueTurnIn" $ do

    it "returns Nothing when the letter points past the ready list" $ do
      let gs = base { gsNPCs   = [mkNPC "alice" []]
                    , gsQuests = [readyQuest "one"]
                    }
      isNothing (stepDialogueTurnIn 0 'C' gs) `shouldBe` True

    it "returns Just when the letter points at a ready quest" $ do
      let gs = base { gsNPCs   = [mkNPC "alice" []]
                    , gsQuests = [readyQuest "one", readyQuest "two"]
                    }
      isJust (stepDialogueTurnIn 0 'A' gs) `shouldBe` True

    it "returns Nothing when there are no ready quests" $ do
      let gs = base { gsNPCs   = [mkNPC "alice" []]
                    , gsQuests = [activeQuest "one"]
                    }
      isNothing (stepDialogueTurnIn 0 'A' gs) `shouldBe` True

  describe "stepQuestLogSelect" $ do

    it "sets the cursor for a valid active-quest index" $ do
      let gs = base
            { gsQuests         = [activeQuest "one", activeQuest "two"]
            , gsQuestLogCursor = Nothing
            }
      gsQuestLogCursor (stepQuestLogSelect 'b' gs) `shouldBe` Just 1

    it "leaves the cursor unchanged for an out-of-range index" $ do
      let gs = base
            { gsQuests         = [activeQuest "one"]
            , gsQuestLogCursor = Nothing
            }
      gsQuestLogCursor (stepQuestLogSelect 'c' gs) `shouldBe` Nothing

    it "only counts active quests, not ready/failed ones" $ do
      -- 'a' is the first active quest even if a ready quest sits
      -- earlier in the list — the quest log only indexes active.
      let gs = base
            { gsQuests         =
                [ readyQuest "done"
                , activeQuest "real target"
                ]
            , gsQuestLogCursor = Nothing
            }
      gsQuestLogCursor (stepQuestLogSelect 'a' gs) `shouldBe` Just 0

    it "refuses to index past the end of the active list" $ do
      let gs = base
            { gsQuests         =
                [ activeQuest "one"
                , readyQuest "not counted"
                ]
            , gsQuestLogCursor = Nothing
            }
      -- only one active quest, so 'b' is out of range
      gsQuestLogCursor (stepQuestLogSelect 'b' gs) `shouldBe` Nothing
