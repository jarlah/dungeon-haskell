{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | LLM-driven quest generation.
--
--   The pipeline is:
--
--   >>> buildQuestPrompt -> sendPrompt -> parseQuestJSON -> Quest
--
--   Every step can fail independently — a network error, a timeout,
--   a non-JSON reply, a JSON object missing a required field. Each
--   failure collapses into 'Nothing' so the caller falls back to the
--   hardcoded quest pool without ever surfacing a crash to the
--   player.
module Game.AI.QuestGen
  ( generateQuest
  , parseQuestJSON
  ) where

import qualified Data.Aeson             as A
import           Data.Aeson             ((.:), (.:?), withObject)
import qualified Data.ByteString.Lazy   as BL
import qualified Data.Text              as T
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as TE

import           Game.AI.Client         (AIClient, sendPrompt)
import           Game.AI.Prompts        (questPrompt)
import           Game.Config            (AIConfig)
import           Game.Logic.Quest       (Quest (..), QuestGoal (..), QuestStatus (..), mkQuest)

-- | Fire a quest request at the LLM backend and parse the reply
--   into a 'Quest'. Returns 'Nothing' on any failure — the caller
--   (NPC placement path in 'Game.GameState.spawnNPCs' or the
--   dialogue opener) falls back to the hardcoded offer list.
generateQuest
  :: AIClient
  -> AIConfig
  -> Int        -- ^ current dungeon depth
  -> Int        -- ^ player level
  -> Int        -- ^ monsters killed so far
  -> IO (Maybe Quest)
generateQuest client cfg depth level kills = do
  let prompt = questPrompt depth level kills
  result <- sendPrompt client cfg prompt
  pure $ case result of
    Left _    -> Nothing
    Right txt -> case parseQuestJSON txt of
      Left _  -> Nothing
      Right q -> Just q

-- | Parse a JSON quest description (as returned by the LLM) into a
--   real 'Quest'. Pure so tests can feed it fixture strings without
--   any IO ceremony. Returns 'Left' with a short human-readable
--   reason on any failure.
parseQuestJSON :: Text -> Either String Quest
parseQuestJSON raw =
  let trimmed = stripCodeFences (T.strip raw)
      bytes   = BL.fromStrict (TE.encodeUtf8 trimmed)
  in case A.eitherDecode bytes of
       Left err -> Left err
       Right (rq :: RawQuest) -> buildQuest rq

-- | The LLM's reply shape, defined as its own record with a custom
--   'FromJSON' instance so we don't have to couple 'Quest' itself
--   (which is used everywhere) to an aeson instance.
data RawQuest = RawQuest
  { rqName        :: !Text
  , rqDescription :: !(Maybe Text)
  , rqGoalType    :: !Text
  , rqGoalValue   :: !(Maybe Int)
  , rqXpReward    :: !Int
  } deriving (Eq, Show)

instance A.FromJSON RawQuest where
  parseJSON = withObject "Quest" $ \o -> RawQuest
    <$> o .:  "name"
    <*> o .:? "description"
    <*> o .:  "goal_type"
    <*> o .:? "goal_value"
    <*> o .:  "xp_reward"

-- | Convert a decoded 'RawQuest' into a real 'Quest'. Clamps the
--   numeric fields to sensible game-balance ranges so a runaway
--   prompt can't create a "kill 500 rats for 1,000,000 XP" quest.
buildQuest :: RawQuest -> Either String Quest
buildQuest rq = do
  goal <- case T.toLower (rqGoalType rq) of
    "kill_monsters" ->
      let n = clamp 1 20 (maybe 1 id (rqGoalValue rq))
      in Right (GoalKillMonsters n)
    "reach_depth" ->
      let d = clamp 2 11 (maybe 2 id (rqGoalValue rq))
      in Right (GoalReachDepth d)
    "kill_boss" ->
      Right GoalKillBoss
    other ->
      Left ("unknown goal_type: " <> T.unpack other)
  let name = sanitizeName (rqName rq)
      reward = clamp 0 500 (rqXpReward rq)
      base = mkQuest name goal
  Right base
    { qReward = reward
      -- Generated quests start as un-accepted /offers/ so they
      -- route through the normal NPC dialogue acceptance flow
      -- rather than showing up in the player's active log
      -- without the player having agreed to take them.
    , qStatus = QuestNotStarted
    }

-- | Strip an optional ```` ```json ... ``` ```` markdown fence the
--   LLM may wrap the JSON in despite the prompt telling it not to.
--   Defensive parsing — the prompt explicitly forbids fences but
--   models ignore that request often enough to be worth handling.
stripCodeFences :: Text -> Text
stripCodeFences t
  | "```" `T.isPrefixOf` t =
      let withoutOpen = T.dropWhile (/= '\n') (T.drop 3 t)
          body        = if T.null withoutOpen then withoutOpen else T.drop 1 withoutOpen
          closed      = fst (T.breakOn "```" body)
      in T.strip closed
  | otherwise = t

-- | Clip a name to something a status panel can render and drop any
--   characters outside the printable ASCII + latin-1 range. Keeps
--   the rest of the game's rendering / persistence layer from
--   having to deal with emoji or control characters the LLM might
--   have smuggled in.
sanitizeName :: Text -> String
sanitizeName t =
  let cleaned = T.filter (\c -> c >= ' ' && c <= '\x7e') (T.strip t)
      trimmed = T.take 40 cleaned
  in if T.null trimmed then "Unnamed Quest" else T.unpack trimmed

-- | Clamp an 'Int' to an inclusive range.
clamp :: Int -> Int -> Int -> Int
clamp lo hi x = max lo (min hi x)
