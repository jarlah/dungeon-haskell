-- | Quest state machine. A 'Quest' carries a 'QuestGoal' (what has
--   to happen), a progress counter, and a status (not-started,
--   active, completed, failed). 'QuestEvent's are fed to
--   'advanceQuest' whenever something happens in the world that a
--   quest might care about — a monster kill, a depth change, a
--   potion drunk. Completed quests are absorbing: nothing moves
--   them away from 'QuestCompleted'. Same for 'QuestFailed'.
--
--   This module is entirely pure. The GameState layer decides when
--   to emit events and displays the quest panel; all of the
--   progress / completion logic lives here where it can be
--   property-tested in isolation.
module Game.Logic.Quest
  ( -- * Types
    QuestGoal(..)
  , QuestStatus(..)
  , QuestEvent(..)
  , Quest(..)
    -- * Construction
  , mkQuest
    -- * State transition
  , advanceQuest
  , advanceAll
    -- * Queries
  , isCompleted
  , isFailed
  , isReady
  , questDescription
  , questProgressLabel
  , fireQuestEvent
  ) where

-- | What a quest is asking the player to accomplish. The fields
--   are interpreted against 'qProgress' in 'advanceQuest'.
data QuestGoal
  = GoalKillMonsters !Int
    -- ^ kill at least N monsters (any kind)
  | GoalReachDepth !Int
    -- ^ reach at least depth D on the dungeon level stack
  | GoalKillBoss
    -- ^ slay the run's boss. Progress is boolean — zero until the
    --   boss dies, then one — so the target is implicitly 1.
  deriving (Eq, Show)

-- | Quest lifecycle. 'QuestActive' and 'QuestNotStarted' are
--   distinct so consumers can surface "not yet accepted" quests
--   differently from "in progress" ones. 'QuestReadyToTurnIn' is
--   the intermediate state a quest enters when its goal is met —
--   no XP is awarded and the quest is not "done" until the player
--   returns to an NPC and hands it in, at which point it flips to
--   'QuestCompleted'. 'QuestCompleted' and 'QuestFailed' are
--   terminal; neither ever transitions back. 'QuestReadyToTurnIn'
--   is also absorbing w.r.t. 'advanceQuest' — further world events
--   don't move it — but the GameState layer *does* move it to
--   'QuestCompleted' via 'turnInQuest'.
data QuestStatus
  = QuestNotStarted
  | QuestActive
  | QuestReadyToTurnIn
  | QuestCompleted
  | QuestFailed
  deriving (Eq, Show)

-- | Things that happen in the world that a quest might react to.
--   Keep this set small and orthogonal — a quest decides for itself
--   whether any given event is relevant.
data QuestEvent
  = EvKilledMonster
  | EvKilledBoss
  | EvEnteredDepth !Int
  deriving (Eq, Show)

-- | A single quest's full state.
data Quest = Quest
  { qName     :: !String
    -- ^ short human-readable name for the quest panel
  , qGoal     :: !QuestGoal
  , qProgress :: !Int
    -- ^ goal-specific progress counter. For 'GoalKillMonsters'
    --   this is a running count; for 'GoalReachDepth' it is the
    --   deepest depth ever visited.
  , qStatus   :: !QuestStatus
  , qReward   :: !Int
    -- ^ XP bounty awarded when the quest is turned in at an NPC.
    --   A quest with @qReward = 0@ still works — it just doesn't
    --   give XP, so it only serves as a journal entry.
  , qGiver    :: !(Maybe Int)
    -- ^ index into 'gsNPCs' of the NPC that originally offered
    --   the quest, stamped at accept time. Turning the quest in
    --   at this NPC awards the full 'qReward'; at any other NPC
    --   the player gets half. 'Nothing' means the quest has no
    --   originating NPC (e.g., a debug-seeded quest) and any NPC
    --   pays full bounty.
  } deriving (Eq, Show)

-- | Build a quest in its initial 'QuestActive' state with zero
--   progress, no XP reward, and no originating NPC. Callers that
--   want a reward or a giver set the fields after construction.
mkQuest :: String -> QuestGoal -> Quest
mkQuest name goal = Quest
  { qName     = name
  , qGoal     = goal
  , qProgress = 0
  , qStatus   = QuestActive
  , qReward   = 0
  , qGiver    = Nothing
  }

-- | Absorbing test for completed (i.e., turned-in) quests.
isCompleted :: Quest -> Bool
isCompleted q = qStatus q == QuestCompleted

-- | Absorbing test for failed quests.
isFailed :: Quest -> Bool
isFailed q = qStatus q == QuestFailed

-- | Test for a quest whose goal has been met but which hasn't yet
--   been turned in at an NPC. Distinct from 'isCompleted': a ready
--   quest has no reward collected yet.
isReady :: Quest -> Bool
isReady q = qStatus q == QuestReadyToTurnIn

-- | Advance a quest by one event. Only quests in 'QuestActive' are
--   affected — 'QuestNotStarted' quests (offered but not yet
--   accepted) silently ignore events, 'QuestReadyToTurnIn' is
--   absorbing w.r.t. 'advanceQuest' (the player still has to
--   hand it in, but the world can't push its progress further),
--   and terminal statuses ('Completed' and 'Failed') are also
--   absorbing. Events that are irrelevant to an active quest's
--   goal are ignored. Progress is monotonic (never decreases) and
--   a quest transitions to 'QuestReadyToTurnIn' the moment its
--   progress reaches its target — the 'QuestCompleted' transition
--   happens in the GameState layer when the reward is collected.
advanceQuest :: QuestEvent -> Quest -> Quest
advanceQuest _ q
  | qStatus q /= QuestActive = q
advanceQuest ev q = case (qGoal q, ev) of
  (GoalKillMonsters target, EvKilledMonster) ->
    let progress' = qProgress q + 1
        status'   = if progress' >= target then QuestReadyToTurnIn else QuestActive
    in q { qProgress = progress', qStatus = status' }
  (GoalReachDepth target, EvEnteredDepth d) ->
    let progress' = max (qProgress q) d
        status'   = if progress' >= target then QuestReadyToTurnIn else QuestActive
    in q { qProgress = progress', qStatus = status' }
  (GoalKillBoss, EvKilledBoss) ->
    q { qProgress = 1, qStatus = QuestReadyToTurnIn }
  _ -> q

-- | Advance every quest in a list by the same event. Convenient
--   for "something happened, notify all quests".
advanceAll :: QuestEvent -> [Quest] -> [Quest]
advanceAll ev = map (advanceQuest ev)

-- | A longer description of what the quest is asking for. Useful
--   for a quest log screen (not used in the minimal UI yet).
questDescription :: Quest -> String
questDescription q = case qGoal q of
  GoalKillMonsters target -> "Kill " ++ show target ++ " monsters."
  GoalReachDepth   target -> "Reach depth " ++ show target ++ "."
  GoalKillBoss            -> "Slay the dragon that rules the deep."

-- | Short progress label for the status panel, e.g. @"3/5"@,
--   @"ready!"@ for quests waiting to be turned in, or @"done"@ for
--   fully-completed ones.
questProgressLabel :: Quest -> String
questProgressLabel q
  | isCompleted q          = "done"
  | isFailed    q          = "failed"
  | qStatus q == QuestReadyToTurnIn = "ready!"
  | otherwise = case qGoal q of
      GoalKillMonsters target -> show (min (qProgress q) target) ++ "/" ++ show target
      GoalReachDepth   target -> show (min (qProgress q) target) ++ "/" ++ show target
      GoalKillBoss            -> show (min (qProgress q) 1) ++ "/1"

fireQuestEvent :: QuestEvent -> [Quest] -> ([Quest], [String])
fireQuestEvent event quests =
  let after = advanceAll event quests
      -- Pair old and new by position; a quest "just became ready"
      -- if it wasn't ready before and is now. Under M12 goal-met
      -- quests flip to 'QuestReadyToTurnIn' (not 'QuestCompleted')
      -- so this is the right place to tell the player their quest
      -- is waiting on a turn-in.
      newlyReady  =
        [ qName q'
        | (q, q') <- zip quests after
        , not (isReady q)
        , isReady q'
        ]
      msgs = [ "Quest ready to turn in: " ++ n ++ "!" | n <- newlyReady ]
  in (after, msgs)