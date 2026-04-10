-- | The AI subsystem's runtime wiring and the Brick-level hooks
--   that drive it. Lifted out of @app/Main.hs@ so that:
--
--   * the pure fold functions ('updateNPCGreet', 'appendQuestToFirstNPC')
--     can be tested directly from HSpec;
--
--   * Main.hs stops being the module that knows how to spin up an
--     AI worker thread, parse greeting prompts, or fold JSON
--     responses into 'GameState';
--
--   * new sites that want to fire AI requests (future milestones)
--     can import 'maybeFireGreeting' / 'maybeFireQuest' /
--     'maybeFireRoomDesc' without cycling through Main.
--
--   The IO-heavy request-firing and response-folding helpers still
--   live in 'EventM' because they have to talk to the Brick event
--   loop, mutate a handful of 'IORef's, and call into the async
--   worker thread. They're not reachable from HSpec directly, but
--   they are now colocated with the data types and helpers they
--   operate on, which is the first step toward making them
--   mockable.
module Game.AI.Runtime
  ( AIRuntime (..)
  , AppEvent (..)
  , startAIRuntime
  , stopAIRuntime
  , maybeFireGreeting
  , maybeFireQuest
  , maybeFireRoomDesc
  , applyAIResponse
  , describeNPCRole
  , updateNPCGreet
  , appendQuestToFirstNPC
  ) where

import Brick (EventM, get, modify)
import qualified Brick.BChan as BChan
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T
import Linear (V2 (..))

import qualified Game.AI.Async as AIAsync
import qualified Game.AI.Client as AIClient
import           Game.AI.Log (AILog, openAILog, closeAILog, logAI)
import qualified Game.AI.Prompts as AIPrompts
import qualified Game.AI.QuestGen as AIQuestGen
import Game.AI.Types (AIRequest (..), AIResponse (..), displayAIError)
import Game.Config (AIConfig (..), GameConfig (..))
import Game.Core
import Game.Logic.Quest (Quest (..), qName)
import Game.Types
  ( DungeonLevel (..), Monster (..), Room (..), Stats (..)
  , monsterName, roomIndexAt
  )
import Game.UI.Types (Name)

--------------------------------------------------------------------
-- Runtime data + Brick event type
--------------------------------------------------------------------

-- | Runtime wiring for the AI subsystem. Bundled together so the
--   event loop only has to close over one value and so shutdown can
--   tear the whole thing down in one 'Control.Exception.bracket'
--   exit action.
--
--   Held outside 'GameState' because none of it is safe to
--   serialize: the worker thread handle is process-local, the config
--   is read-only, and the in-flight set would become stale the
--   moment a save was loaded. A fresh 'AIRuntime' is built once at
--   startup and lives until the app exits.
data AIRuntime = AIRuntime
  { aiCfg        :: !AIConfig
  , aiClient     :: !AIClient.AIClient
  , aiWorker     :: !AIAsync.AIWorker
  , aiLog        :: !AILog
  , aiPending    :: !(IORef [(Int, Int)])
    -- ^ NPC slots (depth, npcIndex) with a greeting request currently
    --   in flight. Kept as a plain list — we only ever have a handful
    --   at once — and consulted before firing a new request so the
    --   worker queue doesn't fill up with duplicates for the same
    --   NPC if the player bumps into them repeatedly while a reply
    --   is still coming back.
  , aiNextToken  :: !(IORef Int)
    -- ^ monotonic token counter used to tag 'AIRequest' values so
    --   the response handler can match them back to their site.
  , aiTokenMap   :: !(IORef [(Int, (Int, Int))])
    -- ^ maps a live correlation token back to the (depth, npcIndex)
    --   it was fired for. Populated when a request is submitted and
    --   drained when the response lands.
  , aiSeenFloors :: !(IORef (Set Int))
    -- ^ set of dungeon depths we've already fired a quest-generation
    --   request for in this process. A depth is added here the
    --   instant the request is submitted (not when it completes), so
    --   a failure doesn't cause an immediate re-fire every keystroke.
    --   Bookkeeping only; the actual generated quests land on the
    --   Quest Master's offer list via 'applyAIResponse'.
  , aiSeenRooms  :: !(IORef (Set (Int, Int)))
    -- ^ set of @(depth, roomIndex)@ slots we've already fired a
    --   room-description request for. Same semantics as
    --   'aiSeenFloors': once fired (success or failure) a room
    --   never fires again in this process, so re-entry doesn't
    --   re-ask the LLM. A save/load round trip resets this because
    --   the IORef is process-local, not serialized.
  }

-- | The Brick custom-event type for this app. 'AIResult' carries a
--   completed 'AIResponse' from the worker thread back to the main
--   event loop via a 'BChan.BChan'; the event loop then folds it
--   into 'GameState' by calling 'applyAIResponse'.
newtype AppEvent
  = AIResult AIResponse
  deriving (Show)

--------------------------------------------------------------------
-- Startup / shutdown
--------------------------------------------------------------------

-- | Spin up the AI runtime. Builds the client, opens the worker
--   thread, and returns everything the event loop needs. The caller
--   is expected to pair this with 'stopAIRuntime' via
--   'Control.Exception.bracket' so the worker thread and any
--   underlying resources are torn down cleanly on exit (including
--   on @Ctrl-C@).
--
--   Takes the Brick 'BChan.BChan' the worker should emit responses
--   into so the event loop picks them up alongside key events.
startAIRuntime :: GameConfig -> BChan.BChan AppEvent -> IO AIRuntime
startAIRuntime gc chan = do
  let cfg = gcAI gc
  alog      <- openAILog
  logAI alog $ "AI runtime starting — enabled=" ++ show (aiEnabled cfg)
                ++ " provider=" ++ show (aiProvider cfg)
                ++ " model=" ++ T.unpack (aiModel cfg)
  client    <- AIClient.newAIClient cfg
  pending   <- newIORef []
  tokRef    <- newIORef 0
  mapRef    <- newIORef []
  seenFloor <- newIORef Set.empty
  seenRoom  <- newIORef Set.empty
  worker    <- AIAsync.startAIWorker client cfg alog $ \resp ->
    BChan.writeBChan chan (AIResult resp)
  pure AIRuntime
    { aiCfg        = cfg
    , aiClient     = client
    , aiWorker     = worker
    , aiLog        = alog
    , aiPending    = pending
    , aiNextToken  = tokRef
    , aiTokenMap   = mapRef
    , aiSeenFloors = seenFloor
    , aiSeenRooms  = seenRoom
    }

-- | Tear down the AI runtime. Idempotent — safe to call during
--   cleanup even if startup partially failed.
stopAIRuntime :: AIRuntime -> IO ()
stopAIRuntime rt = do
  logAI (aiLog rt) "AI runtime stopping"
  AIAsync.stopAIWorker (aiWorker rt)
  AIClient.closeAIClient (aiClient rt)
  closeAILog (aiLog rt)

--------------------------------------------------------------------
-- Request firing — called after every keystroke
--------------------------------------------------------------------

-- | If the player is currently in a dialogue with an NPC that has
--   no cached AI greeting yet, fire a greeting request. Guards
--   against duplicate fires via 'aiPending' so that rapid reopens
--   of the same dialogue produce at most one inflight request.
--
--   Called after every keystroke — most of the time it's a no-op
--   because either the dialogue is closed, AI is disabled, or the
--   greeting is already cached. The cost of checking is a few
--   pattern matches.
maybeFireGreeting :: AIRuntime -> EventM Name GameState ()
maybeFireGreeting rt
  | not (aiEnabled (aiCfg rt)) = pure ()
  | otherwise = do
      gs <- get
      case gsDialogue gs of
        Nothing      -> pure ()
        Just npcIdx  -> case drop npcIdx (gsNPCs gs) of
          []        -> pure ()
          (npc : _) -> case npcAIGreet npc of
            Just _  -> pure ()  -- already cached
            Nothing -> do
              let depth = dlDepth (gsLevel gs)
                  slot  = (depth, npcIdx)
              already <- liftIO $ atomicModifyIORef' (aiPending rt) $ \ps ->
                if slot `elem` ps
                  then (ps, True)
                  else (slot : ps, False)
              unless already $ liftIO $ do
                tok <- atomicModifyIORef' (aiNextToken rt) $ \n -> (n + 1, n + 1)
                atomicModifyIORef' (aiTokenMap rt) $ \m -> ((tok, slot) : m, ())
                let prompt = AIPrompts.greetingPrompt
                        (T.pack (npcName npc))
                        (T.pack (describeNPCRole npc))
                        depth
                AIAsync.requestAI (aiWorker rt) (ReqGreeting tok prompt)

-- | Flatten an NPC into a short role string the greeting prompt can
--   key its flavor off. For the MVP every NPC is the Quest Master,
--   so this is essentially a constant — but it's a single point of
--   change when new NPC kinds land.
describeNPCRole :: NPC -> String
describeNPCRole _ = "Quest Master"

-- | Fire a quest-generation request for the current dungeon floor if
--   we haven't already. Runs on every keystroke — most calls are
--   no-ops because AI is disabled or the floor is already in
--   'aiSeenFloors'. The first keystroke on a fresh floor is the
--   one that actually submits the request.
--
--   The response lands back in 'applyAIResponse' as a 'RespQuest',
--   where the JSON is parsed into a 'Quest' and appended to the
--   Quest Master's offer list. The player discovers it next time
--   they open the dialogue.
maybeFireQuest :: AIRuntime -> EventM Name GameState ()
maybeFireQuest rt
  | not (aiEnabled (aiCfg rt)) = pure ()
  | otherwise = do
      gs <- get
      let depth = dlDepth (gsLevel gs)
      fire <- liftIO $ atomicModifyIORef' (aiSeenFloors rt) $ \s ->
        if Set.member depth s
          then (s, False)
          else (Set.insert depth s, True)
      when fire $ liftIO $ do
        tok <- atomicModifyIORef' (aiNextToken rt) $ \n -> (n + 1, n + 1)
        let prompt = AIPrompts.questPrompt
                       depth
                       (sLevel (gsPlayerStats gs))
                       0   -- ^ monsters-killed counter not tracked; pass 0 for flavor
        AIAsync.requestAI (aiWorker rt) (ReqQuest tok prompt)

-- | Fire a room-description request for the room the player is
--   currently standing in, if we haven't already asked about it on
--   this floor. Dedup is keyed on @(depth, roomIndex)@ via
--   'aiSeenRooms' so walking in and out of a room doesn't re-ask
--   the LLM, and descending to a previously-visited floor doesn't
--   either (the set is process-local, not serialized).
--
--   Any pending description from the previous room is hidden the
--   instant a new request fires, so the old panel can't linger on
--   top of the new room while the reply is in flight.
maybeFireRoomDesc :: AIRuntime -> EventM Name GameState ()
maybeFireRoomDesc rt
  | not (aiEnabled (aiCfg rt)) = pure ()
  | otherwise = do
      gs <- get
      let depth = dlDepth (gsLevel gs)
          rooms = dlRooms (gsLevel gs)
      case roomIndexAt rooms (gsPlayerPos gs) of
        Nothing      -> pure ()
        Just roomIdx -> do
          let slot = (depth, roomIdx)
          fire <- liftIO $ atomicModifyIORef' (aiSeenRooms rt) $ \s ->
            if Set.member slot s
              then (s, False)
              else (Set.insert slot s, True)
          when fire $ do
            -- Hide any stale description from the previous room so
            -- the old panel doesn't sit on top of the new room while
            -- the new reply is in flight.
            modify (\s -> s { gsRoomDescVisible = False })
            let room = rooms !! roomIdx
                monsterNames =
                  [ T.pack (monsterName (mKind m))
                  | m <- gsMonsters gs
                  , let V2 mx my = mPos m
                  , mx >= rX room, mx < rX room + rW room
                  , my >= rY room, my < rY room + rH room
                  ]
            liftIO $ do
              tok <- atomicModifyIORef' (aiNextToken rt) $ \n -> (n + 1, n + 1)
              atomicModifyIORef' (aiTokenMap rt) $ \m -> ((tok, slot) : m, ())
              let prompt = AIPrompts.roomDescPrompt
                             (rW room) (rH room) depth monsterNames
              AIAsync.requestAI (aiWorker rt) (ReqRoomDesc tok prompt)

--------------------------------------------------------------------
-- Response folding
--------------------------------------------------------------------

-- | Fold a completed AI response into 'GameState'. For greetings
--   this is: find the NPC the request was fired for and stamp the
--   cleaned reply into its 'npcAIGreet' field. For quest generation
--   the JSON is parsed and the resulting 'Quest' is appended to the
--   first NPC's offer list. Room-description replies paste into
--   'gsRoomDesc' iff the player is still on the same tile.
--
--   Unknown or stale tokens (for example, a response that arrives
--   after a save was loaded and the token map was wiped) are
--   silently dropped — they're cosmetic.
applyAIResponse :: AIRuntime -> AIResponse -> EventM Name GameState ()
applyAIResponse rt resp = case resp of
  RespGreeting tok (Right greetTxt) -> do
    mSlot <- liftIO $ atomicModifyIORef' (aiTokenMap rt) $ \m ->
      case lookup tok m of
        Just s  -> (filter ((/= tok) . fst) m, Just s)
        Nothing -> (m, Nothing)
    liftIO $ atomicModifyIORef' (aiPending rt) $ \ps ->
      case mSlot of
        Just s  -> (filter (/= s) ps, ())
        Nothing -> (ps, ())
    case mSlot of
      Nothing -> pure ()
      Just (depth, npcIdx) -> do
        gs <- get
        -- Only fold the reply in if the player is still on the same
        -- floor. Descending invalidates the NPC index, so a late
        -- reply for a previous floor's NPC would otherwise paint
        -- itself onto whoever is now at that index.
        when (dlDepth (gsLevel gs) == depth) $
          modify (updateNPCGreet npcIdx (T.unpack greetTxt))
  RespGreeting tok (Left err) -> do
    liftIO $ logAI (aiLog rt) $ "greeting response failed: "
              ++ T.unpack (displayAIError err)
    -- Failure: clear the bookkeeping so a retry is possible next
    -- time the player re-enters the dialogue. The greeting field
    -- stays 'Nothing', so the fallback line keeps showing.
    mSlot <- liftIO $ atomicModifyIORef' (aiTokenMap rt) $ \m ->
      case lookup tok m of
        Just s  -> (filter ((/= tok) . fst) m, Just s)
        Nothing -> (m, Nothing)
    liftIO $ atomicModifyIORef' (aiPending rt) $ \ps ->
      case mSlot of
        Just s  -> (filter (/= s) ps, ())
        Nothing -> (ps, ())
  RespQuest _ (Right body) ->
    -- Parse the LLM's JSON reply and, on success, append the quest
    -- to the Quest Master's offer list so the player finds it the
    -- next time they open the dialogue. Parse failures drop silently
    -- — the player still has the hardcoded offers to pick from.
    case AIQuestGen.parseQuestJSON body of
      Left _  -> pure ()
      Right q -> do
        modify (appendQuestToFirstNPC q)
        -- Soft in-world hint so the player knows something new is
        -- available without having to revisit the Quest Master on
        -- every descent. Only fires on a successful parse so a
        -- failed request doesn't leak into the log.
        modify $ \gs -> gs
          { gsMessages =
              ("The Quest Master calls on you: \"" ++ qName q ++ "\".")
              : gsMessages gs
          }
  RespQuest _ (Left err) ->
    liftIO $ logAI (aiLog rt) $ "quest response failed: "
              ++ T.unpack (displayAIError err)
  RespRoomDesc tok (Right descTxt) -> do
    -- Pop the slot this token was fired for, if any. A missing
    -- token is silently dropped — likely a load/reset cleared the
    -- map between request and reply.
    mSlot <- liftIO $ atomicModifyIORef' (aiTokenMap rt) $ \m ->
      case lookup tok m of
        Just s  -> (filter ((/= tok) . fst) m, Just s)
        Nothing -> (m, Nothing)
    case mSlot of
      Nothing -> pure ()
      Just (depth, roomIdx) -> do
        gs <- get
        let curDepth = dlDepth (gsLevel gs)
            curRoom  = roomIndexAt (dlRooms (gsLevel gs)) (gsPlayerPos gs)
        -- Only apply if the player is still in the same room on the
        -- same floor. Otherwise the reply is stale — they've already
        -- walked off and pasting it now would describe the wrong
        -- room. Drop silently.
        when (curDepth == depth && curRoom == Just roomIdx) $
          modify $ \s -> s
            { gsRoomDesc        = Just (T.unpack descTxt)
            , gsRoomDescVisible = True
            }
  RespRoomDesc tok (Left err) -> do
    liftIO $ logAI (aiLog rt) $ "room-desc response failed: "
              ++ T.unpack (displayAIError err)
    -- Failure: just drain the token map so the slot counter doesn't
    -- leak. The description stays 'Nothing' and the panel never
    -- appears; the player sees no difference from AI being disabled.
    liftIO $ atomicModifyIORef' (aiTokenMap rt) $ \m ->
      (filter ((/= tok) . fst) m, ())

--------------------------------------------------------------------
-- Pure state helpers — testable directly from HSpec
--------------------------------------------------------------------

-- | Stamp a cleaned greeting string into the NPC at the given index.
--   No-op if the index is out of range (stale token, NPC despawned).
updateNPCGreet :: Int -> String -> GameState -> GameState
updateNPCGreet idx g gs =
  let updated = zipWith (\i n -> if i == idx then n { npcAIGreet = Just g } else n)
                        [0 ..]
                        (gsNPCs gs)
  in gs { gsNPCs = updated }

-- | Append a newly generated quest to the first NPC's offer list.
--   In the MVP the Quest Master lives at index 0 of 'gsNPCs' and
--   is the only quest giver in the game, so "first NPC" is a safe
--   proxy for "Quest Master". If 'gsNPCs' is empty the quest is
--   silently dropped — the only way that happens is an unexpected
--   load order, and a lost cosmetic quest is preferable to crashing.
appendQuestToFirstNPC :: Quest -> GameState -> GameState
appendQuestToFirstNPC q gs = case gsNPCs gs of
  []           -> gs
  (npc : rest) ->
    gs { gsNPCs = npc { npcOffers = npcOffers npc ++ [q] } : rest }
