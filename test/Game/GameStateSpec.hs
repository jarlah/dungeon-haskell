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
import Game.Logic.Dungeon (defaultLevelConfig)
import Game.Logic.MonsterAI (chebyshev)
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
  , gsAwaitingDirection = Nothing
  , gsCheatsUsed        = False
  , gsNextKeyId         = 0
  , gsPendingKeys       = []
  , gsLockedDoorPrompt  = Nothing
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

  -- ----------------------------------------------------------------
  -- Milestone 15 / Step 1: bump-to-open for closed doors. The
  -- fixtures here stamp a closed door into tinyRoom so the turn is
  -- fully deterministic and doesn't depend on the BSP generator.
  -- ----------------------------------------------------------------

  it "bumping a closed door opens it and the player stays put" $ do
    -- Put a closed door at (2,1), directly north of a player at
    -- (2,2). tinyRoom is 5 wide, so index = 1*5 + 2 = 7.
    let doorPos  = V2 2 1
        doorIdx  = 1 * 5 + 2
        withDoor = tinyRoom
          { dlTiles = dlTiles tinyRoom V.// [(doorIdx, Door Closed)] }
        gs  = (mkFixture 1 (V2 2 2) overpoweredPlayer [])
                { gsLevel = withDoor }
        gs' = applyAction (Move N) gs
    gsPlayerPos gs' `shouldBe` V2 2 2
    tileAt (gsLevel gs') doorPos `shouldBe` Just (Door Open)

  -- ----------------------------------------------------------------
  -- Milestone 15 / Step 2: locked doors.
  -- ----------------------------------------------------------------

  it "bumping a locked door without the matching key raises the prompt and costs no turn" $ do
    let kid      = KeyId 0
        doorPos  = V2 2 1
        doorIdx  = 1 * 5 + 2
        withDoor = tinyRoom
          { dlTiles = dlTiles tinyRoom V.// [(doorIdx, Door (Locked kid))] }
        rat      = ratAt (V2 3 3)
        gs       = (mkFixture 1 (V2 2 2) overpoweredPlayer [rat])
                     { gsLevel = withDoor }
        gs'      = applyAction (Move N) gs
    -- Prompt set, door untouched, player unmoved.
    gsLockedDoorPrompt gs' `shouldBe` Just (keyName kid)
    tileAt (gsLevel gs') doorPos `shouldBe` Just (Door (Locked kid))
    gsPlayerPos gs' `shouldBe` V2 2 2
    -- Monster phase did NOT run: rat still at (3,3).
    map mPos (gsMonsters gs') `shouldBe` [V2 3 3]

  it "bumping a locked door with the matching key consumes it and opens the door" $ do
    let kid      = KeyId 0
        doorPos  = V2 2 1
        doorIdx  = 1 * 5 + 2
        withDoor = tinyRoom
          { dlTiles = dlTiles tinyRoom V.// [(doorIdx, Door (Locked kid))] }
        inv      = emptyInventory { invItems = [IKey kid] }
        gs       = (mkFixture 1 (V2 2 2) overpoweredPlayer [])
                     { gsLevel = withDoor, gsInventory = inv }
        gs'      = applyAction (Move N) gs
    -- Door is now open, key is gone, prompt stayed unset.
    tileAt (gsLevel gs') doorPos `shouldBe` Just (Door Open)
    invItems (gsInventory gs') `shouldBe` []
    gsLockedDoorPrompt gs' `shouldBe` Nothing
    -- Same semantics as bumping a closed door: player stays put this
    -- turn (the open consumes the move) but the turn advanced.
    gsPlayerPos gs' `shouldBe` V2 2 2

  it "a non-matching key does not open a locked door" $ do
    let kidLock  = KeyId 0
        kidBag   = KeyId 1
        doorPos  = V2 2 1
        doorIdx  = 1 * 5 + 2
        withDoor = tinyRoom
          { dlTiles = dlTiles tinyRoom V.// [(doorIdx, Door (Locked kidLock))] }
        inv      = emptyInventory { invItems = [IKey kidBag] }
        gs       = (mkFixture 1 (V2 2 2) overpoweredPlayer [])
                     { gsLevel = withDoor, gsInventory = inv }
        gs'      = applyAction (Move N) gs
    tileAt (gsLevel gs') doorPos `shouldBe` Just (Door (Locked kidLock))
    gsLockedDoorPrompt gs' `shouldBe` Just (keyName kidLock)
    invItems (gsInventory gs') `shouldBe` [IKey kidBag]

  it "a door opened on turn N is walkable on turn N+1" $ do
    -- Same setup, but this time take a second move step after the
    -- bump — the player should now walk through the opened door.
    let doorPos  = V2 2 1
        doorIdx  = 1 * 5 + 2
        withDoor = tinyRoom
          { dlTiles = dlTiles tinyRoom V.// [(doorIdx, Door Closed)] }
        gs0 = (mkFixture 1 (V2 2 2) overpoweredPlayer [])
                { gsLevel = withDoor }
        gs1 = applyAction (Move N) gs0   -- opens the door
        gs2 = applyAction (Move N) gs1   -- steps onto the door
    gsPlayerPos gs2 `shouldBe` doorPos

  -- ----------------------------------------------------------------
  -- Milestone 16 follow-up: directional CloseDoor action. Uses the
  -- same tinyRoom-with-stamped-door pattern as the bump-to-open
  -- tests so the turn remains fully deterministic.
  -- ----------------------------------------------------------------

  it "CloseDoor closes an adjacent open door and costs a turn" $ do
    -- Open door at (2,1), player at (1,1). A rat is placed at (3,3)
    -- — Chebyshev 2 from the player, so monsterIntent picks MiMove,
    -- not MiAttack, and we can detect that the monster phase ran by
    -- observing its position change.
    let doorPos  = V2 2 1
        doorIdx  = 1 * 5 + 2
        withDoor = tinyRoom
          { dlTiles = dlTiles tinyRoom V.// [(doorIdx, Door Open)] }
        rat      = ratAt (V2 3 3)
        gs       = (mkFixture 1 (V2 1 1) overpoweredPlayer [rat])
                     { gsLevel = withDoor }
        gs'      = applyAction (CloseDoor E) gs
    tileAt (gsLevel gs') doorPos `shouldBe` Just (Door Closed)
    -- Rat moved toward the player: a successful close advanced the turn.
    map mPos (gsMonsters gs') /= [V2 3 3] `shouldBe` True

  it "CloseDoor on an empty floor tile is a no-op that does NOT cost a turn" $ do
    -- No door east of the player. The attempt must fail with a
    -- message but leave monsters un-advanced.
    let rat = ratAt (V2 3 3)
        gs  = mkFixture 1 (V2 1 1) overpoweredPlayer [rat]
        gs' = applyAction (CloseDoor E) gs
    -- Monster didn't move — monster phase was skipped.
    map mPos (gsMonsters gs') `shouldBe` [V2 3 3]
    -- And an explanatory message landed at the top of the log.
    case gsMessages gs' of
      (m : _) -> m `shouldBe` "There is no door there to close."
      []      -> expectationFailure "expected a message"

  it "CloseDoor on an already-closed door is a no-op with a message" $ do
    let doorIdx  = 1 * 5 + 2
        withDoor = tinyRoom
          { dlTiles = dlTiles tinyRoom V.// [(doorIdx, Door Closed)] }
        gs       = (mkFixture 1 (V2 2 2) overpoweredPlayer [])
                     { gsLevel = withDoor }
        gs'      = applyAction (CloseDoor N) gs
    case gsMessages gs' of
      (m : _) -> m `shouldBe` "That door is already closed."
      []      -> expectationFailure "expected a message"

  it "CloseDoor refuses when a monster stands on the open door" $ do
    -- Door at (2,1), open, with a rat standing on it. The close
    -- attempt must fail with 'Something is in the way.' and the
    -- door must remain open.
    let doorPos  = V2 2 1
        doorIdx  = 1 * 5 + 2
        withDoor = tinyRoom
          { dlTiles = dlTiles tinyRoom V.// [(doorIdx, Door Open)] }
        rat      = ratAt doorPos
        gs       = (mkFixture 1 (V2 2 2) overpoweredPlayer [rat])
                     { gsLevel = withDoor }
        gs'      = applyAction (CloseDoor N) gs
    tileAt (gsLevel gs') doorPos `shouldBe` Just (Door Open)
    case gsMessages gs' of
      (m : _) -> m `shouldBe` "Something is in the way."
      []      -> expectationFailure "expected a message"

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
  -- Monster turns: exercise the full pipeline through
  -- processMonsters -> monsterIntent -> monsterAttack. Existing
  -- tests mostly kill the rat in one hit, so the rat never gets a
  -- turn; these fill that gap by letting the player skip their
  -- own attack and observing what the rat does next.
  -- ----------------------------------------------------------------

  it "a non-adjacent rat advances toward the player on Wait" $ do
    -- Player at (1,1), rat at (3,3) — Chebyshev distance 2. After
    -- a Wait the rat's monsterIntent should return MiMove and the
    -- new position should be Chebyshev 1 from the player. No dice
    -- are involved in the movement path, so this is deterministic.
    let rat = ratAt (V2 3 3)
        gs  = mkFixture 1 (V2 1 1) overpoweredPlayer [rat]
        gs' = applyAction Wait gs
        newRatPos = case gsMonsters gs' of
          (r : _) -> mPos r
          []      -> error "rat vanished unexpectedly"
    -- The rat should have stepped toward the player. Any of the
    -- three diagonal/ortho steps toward (1,1) reduces Chebyshev
    -- to 1; (2,2) is the preferred pick but (2,3)/(3,2) also work
    -- depending on how bestNeighbor breaks ties.
    chebyshev newRatPos (V2 1 1) `shouldBe` 1

  it "an adjacent rat eventually hurts the player across several Waits" $ do
    -- Player at (2,2), rat at (2,1). On each Wait the rat rolls an
    -- attack from 'gsRng'. With a 0-defense player, the rat's hit
    -- threshold sits at 8 on a d20 — roughly 65% hit per turn — so
    -- *some* Wait in a handful of turns will land. Rather than
    -- pinning a specific seed (and being brittle against any
    -- future RNG-consumption reshuffle), we iterate Waits and
    -- assert that HP drops within a bounded window. The odds of
    -- 12 consecutive misses at 35% each are < 1e-5, which is fine
    -- for a deterministic test seed. This covers the full
    -- processMonster -> monsterIntent (MiAttack) -> monsterAttack
    -- pipeline including the EvPlayerHurt emission path.
    let weakPlayer = overpoweredPlayer { sHP = 20, sDefense = 0 }
        rat        = ratAt (V2 2 1)
        gs0        = mkFixture 1 (V2 2 2) weakPlayer [rat]
        gss        = iterate (applyAction Wait) gs0
        damaged    = take 12 (drop 1 gss)
        anyHit     = any (\g -> sHP (gsPlayerStats g) < sHP weakPlayer) damaged
        anyEvent   = any (\g -> EvPlayerHurt `elem` gsEvents g) damaged
    anyHit   `shouldBe` True
    anyEvent `shouldBe` True

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

  -- ----------------------------------------------------------------
  -- Wizard / cheat-mode split.
  --
  -- 'applyCommand' must stamp 'gsCheatsUsed' the moment any wizard
  -- command runs, so a save written after the command can be
  -- reliably distinguished from a clean run. The stamp is one-way
  -- and survives a save/load round trip.
  -- ----------------------------------------------------------------

  it "gsCheatsUsed defaults to False on a fresh fixture" $ do
    let gs = mkFixture 1 (V2 2 2) overpoweredPlayer []
    gsCheatsUsed gs `shouldBe` False

  it "applyCommand CmdHeal flips gsCheatsUsed to True" $ do
    let gs  = (mkFixture 1 (V2 2 2) overpoweredPlayer [])
                { gsPlayerStats = overpoweredPlayer { sHP = 1 } }
        gs' = applyCommand CmdHeal gs
    gsCheatsUsed gs' `shouldBe` True

  it "applyCommand CmdReveal flips gsCheatsUsed to True" $ do
    let gs  = mkFixture 1 (V2 2 2) overpoweredPlayer []
        gs' = applyCommand CmdReveal gs
    gsCheatsUsed gs' `shouldBe` True

  it "gsCheatsUsed, once True, survives an encode/decode round-trip" $ do
    let gs0 = (mkFixture 1 (V2 2 2) overpoweredPlayer [])
                { gsCheatsUsed = True }
    case decodeSave (encodeSave gs0) of
      Right gs' -> gsCheatsUsed gs' `shouldBe` True
      Left e    -> expectationFailure ("decode failed: " ++ show e)

  it "gsCheatsUsed = False round-trips as False" $ do
    let gs0 = mkFixture 1 (V2 2 2) overpoweredPlayer []
    case decodeSave (encodeSave gs0) of
      Right gs' -> gsCheatsUsed gs' `shouldBe` False
      Left e    -> expectationFailure ("decode failed: " ++ show e)

  -- ----------------------------------------------------------------
  -- Softlock safety: when 'newGame' mints a locked door on depth 1,
  -- the matching key must be placed on the spawn-side of that door.
  -- We scan many seeds rather than asserting against a single known
  -- lock-bearing seed, so drift in the generator doesn't silently
  -- hide a regression.
  -- ----------------------------------------------------------------

  describe "newGame locked-door key reachability" $
    it "every minted key lands on a tile reachable from spawn without opening any lock" $ do
      -- 200 seeds is plenty to hit plenty of lock rolls (~50% rate)
      -- while staying well under a second.
      let seeds         = [0 .. 199 :: Int]
          results       = map run seeds
          failures      = filter (not . snd) results
      failures `shouldBe` []
      where
        run seed =
          let gs        = newGame (mkStdGen seed) defaultLevelConfig
              dl        = gsLevel gs
              start     = gsPlayerPos gs
              reachable = keylessFlood dl start
              keyTiles  = [ p | (p, IKey _) <- gsItemsOnFloor gs ]
              hasLock   = any isLockedDoor (V.toList (dlTiles dl))
              -- Either the level has no lock (trivially satisfied),
              -- or every dropped key is on the spawn-side set.
              ok = not hasLock
                || all (`Set.member` reachable) keyTiles
          in (seed, ok)

        isLockedDoor (Door (Locked _)) = True
        isLockedDoor _                 = False

        -- Local flood fill mirroring 'spawnSideReachable' — treats
        -- walls and locked doors as impassable, every other tile as
        -- passable.
        keylessFlood dl start = go (Set.singleton start) [start]
          where
            passable p = case tileAt dl p of
              Just Wall              -> False
              Just (Door (Locked _)) -> False
              Just _                 -> True
              Nothing                -> False
            go visited []       = visited
            go visited (p : qs) =
              let ns    = [ p + V2 0 (-1), p + V2 0 1, p + V2 1 0, p + V2 (-1) 0 ]
                  fresh = [ n | n <- ns, not (Set.member n visited), passable n ]
              in go (foldr Set.insert visited fresh) (qs ++ fresh)

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
