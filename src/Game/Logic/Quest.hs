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
  , questDescription
  , questProgressLabel
  ) where

-- | What a quest is asking the player to accomplish. The fields
--   are interpreted against 'qProgress' in 'advanceQuest'.
data QuestGoal
  = GoalKillMonsters !Int
    -- ^ kill at least N monsters (any kind)
  | GoalReachDepth !Int
    -- ^ reach at least depth D on the dungeon level stack
  deriving (Eq, Show)

-- | Quest lifecycle. 'QuestActive' and 'QuestNotStarted' are
--   distinct so consumers can surface "not yet accepted" quests
--   differently from "in progress" ones — useful for future
--   quest-giver NPCs. 'QuestCompleted' and 'QuestFailed' are
--   terminal; neither ever transitions back.
data QuestStatus
  = QuestNotStarted
  | QuestActive
  | QuestCompleted
  | QuestFailed
  deriving (Eq, Show)

-- | Things that happen in the world that a quest might react to.
--   Keep this set small and orthogonal — a quest decides for itself
--   whether any given event is relevant.
data QuestEvent
  = EvKilledMonster
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
  } deriving (Eq, Show)

-- | Build a quest in its initial 'QuestActive' state with zero
--   progress.
mkQuest :: String -> QuestGoal -> Quest
mkQuest name goal = Quest
  { qName     = name
  , qGoal     = goal
  , qProgress = 0
  , qStatus   = QuestActive
  }

-- | Absorbing test for completed quests.
isCompleted :: Quest -> Bool
isCompleted q = qStatus q == QuestCompleted

-- | Absorbing test for failed quests.
isFailed :: Quest -> Bool
isFailed q = qStatus q == QuestFailed

-- | Advance a quest by one event. Terminal statuses ('Completed'
--   and 'Failed') are absorbing — events never move them. Events
--   that are irrelevant to a quest's goal are ignored. Progress is
--   monotonic (never decreases) and a quest transitions to
--   'QuestCompleted' the moment its progress reaches its target.
advanceQuest :: QuestEvent -> Quest -> Quest
advanceQuest _ q
  | qStatus q == QuestCompleted || qStatus q == QuestFailed = q
advanceQuest ev q = case (qGoal q, ev) of
  (GoalKillMonsters target, EvKilledMonster) ->
    let progress' = qProgress q + 1
        status'   = if progress' >= target then QuestCompleted else QuestActive
    in q { qProgress = progress', qStatus = status' }
  (GoalReachDepth target, EvEnteredDepth d) ->
    let progress' = max (qProgress q) d
        status'   = if progress' >= target then QuestCompleted else QuestActive
    in q { qProgress = progress', qStatus = status' }
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

-- | Short progress label for the status panel, e.g. @"3/5"@ or
--   @"done"@ for completed quests.
questProgressLabel :: Quest -> String
questProgressLabel q
  | isCompleted q = "done"
  | isFailed    q = "failed"
  | otherwise = case qGoal q of
      GoalKillMonsters target -> show (min (qProgress q) target) ++ "/" ++ show target
      GoalReachDepth   target -> show (min (qProgress q) target) ++ "/" ++ show target
