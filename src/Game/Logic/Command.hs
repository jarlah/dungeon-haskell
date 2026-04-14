-- | Pure parser for the slash-command prompt.
--
--   The prompt layer (render + input) lives in 'Main' / 'Render';
--   everything reachable through the prompt is defined here as a
--   plain ADT and a pure @parseCommand@ function so it can be
--   unit-tested without touching Brick.
--
--   Commands fall into two buckets:
--
--   * **Safe commands** — UI shortcuts (@/help@, @/save@, @/quit@,
--     ...) that don't modify the game world in ways the normal
--     keybindings wouldn't. Always available. 'Main' dispatches
--     them inline because several of them need to do IO (open a
--     save picker, write a quicksave file).
--
--   * **Wizard / cheat commands** — hacks that modify the world
--     directly (reveal map, heal, spawn monsters, teleport, ...).
--     Only allowed when the game was launched with @--wizard@.
--     Dispatched through 'Game.Core.applyCommand', which also
--     stamps 'gsCheatsUsed' the moment any of them runs so the
--     save file can be flagged as a wizard-touched run.
module Game.Logic.Command
  ( Command(..)
  , parseCommand
  , isCheatCommand
  ) where

import Data.Char (toLower)
import Linear (V2(..))
import Text.Read (readMaybe)

import Game.Types (MonsterKind(..), Pos)

-- | A parsed prompt command. Adding a new command is a matter of
--   adding a constructor here, a parser case in 'parseCommand',
--   and a dispatch arm in either 'handlePromptKey' (safe commands
--   that need IO / modal state) or 'Game.Core.applyCommand'
--   (pure wizard commands).
data Command
  -- Safe UI commands — always available.
  = CmdHelp
    -- ^ open the help modal
  | CmdQuit
    -- ^ open the quit-confirmation modal
  | CmdInventory
    -- ^ open the inventory modal
  | CmdQuests
    -- ^ open the quest log modal
  | CmdWait
    -- ^ pass a turn (equivalent to the Wait action key)
  | CmdSave
    -- ^ open the save picker modal
  | CmdLoad
    -- ^ open the load picker modal
  | CmdQuicksave
    -- ^ write the current state to the quicksave slot
  | CmdQuickload
    -- ^ load the quicksave slot
  | CmdVolume
    -- ^ open the volume mixer modal
  -- Wizard / cheat commands — gated on the @--wizard@ CLI flag.
  | CmdReveal
    -- ^ mark every tile on the current level as explored
  | CmdHeal
    -- ^ restore the player to full HP
  | CmdKillAll
    -- ^ clear every monster from the current level
  | CmdTeleport !Pos
    -- ^ move the player to an arbitrary walkable tile
  | CmdSpawn !MonsterKind
    -- ^ drop a monster adjacent to the player
  | CmdXP !Int
    -- ^ grant N experience points (pairs with the level curve)
  | CmdDescend
    -- ^ force-descend without standing on stairs
  | CmdAscend
    -- ^ force-ascend without standing on stairs (refused at depth 1)
  deriving (Eq, Show)

-- | 'True' if the command is a wizard / cheat helper that modifies
--   the game world directly. Used by the prompt dispatch to reject
--   these commands unless the player launched the game with
--   @--wizard@, and by 'Game.Core.applyCommand' to stamp
--   'gsCheatsUsed' on the save.
isCheatCommand :: Command -> Bool
isCheatCommand cmd = case cmd of
  -- Safe UI shortcuts.
  CmdHelp       -> False
  CmdQuit       -> False
  CmdInventory  -> False
  CmdQuests     -> False
  CmdWait       -> False
  CmdSave       -> False
  CmdLoad       -> False
  CmdQuicksave  -> False
  CmdQuickload  -> False
  CmdVolume     -> False
  -- Wizard cheats.
  CmdReveal     -> True
  CmdHeal       -> True
  CmdKillAll    -> True
  CmdTeleport _ -> True
  CmdSpawn    _ -> True
  CmdXP       _ -> True
  CmdDescend    -> True
  CmdAscend     -> True

-- | Parse a raw prompt buffer into a 'Command'. Leading whitespace
--   and a single leading @/@ are tolerated so the user can type
--   either @reveal@ or @/reveal@. The verb is matched
--   case-insensitively; arguments keep their case (but the only
--   arguments we currently parse are integers and monster kinds,
--   which are matched case-insensitively as well).
parseCommand :: String -> Either String Command
parseCommand raw =
  case words (stripLeadSlash (dropWhile (== ' ') raw)) of
    [] -> Left "empty command"
    (w : args) -> case map toLower w of
      -- Safe UI commands.
      "help"       -> nullary CmdHelp       args
      "quit"       -> nullary CmdQuit       args
      "exit"       -> nullary CmdQuit       args
      "inv"        -> nullary CmdInventory  args
      "inventory"  -> nullary CmdInventory  args
      "quests"     -> nullary CmdQuests     args
      "questlog"   -> nullary CmdQuests     args
      "wait"       -> nullary CmdWait       args
      "rest"       -> nullary CmdWait       args
      "save"       -> nullary CmdSave       args
      "load"       -> nullary CmdLoad       args
      "quicksave"  -> nullary CmdQuicksave  args
      "qs"         -> nullary CmdQuicksave  args
      "quickload"  -> nullary CmdQuickload  args
      "ql"         -> nullary CmdQuickload  args
      "volume"     -> nullary CmdVolume     args
      "vol"        -> nullary CmdVolume     args
      "mixer"      -> nullary CmdVolume     args
      -- Wizard cheats.
      "reveal"    -> nullary CmdReveal args
      "heal"      -> nullary CmdHeal args
      "kill-all"  -> nullary CmdKillAll args
      "killall"   -> nullary CmdKillAll args
      "descend"   -> nullary CmdDescend args
      "ascend"    -> nullary CmdAscend args
      "tp"        -> parseTeleport args
      "teleport"  -> parseTeleport args
      "spawn"     -> parseSpawn args
      "xp"        -> parseXP args
      other       -> Left ("unknown command: " ++ other)
  where
    stripLeadSlash ('/' : rest) = rest
    stripLeadSlash s            = s

-- | Helper for commands that take no arguments; errors out if the
--   user typed extra tokens so mistakes are visible.
nullary :: Command -> [String] -> Either String Command
nullary c [] = Right c
nullary _ xs = Left ("command takes no arguments, got: " ++ unwords xs)

parseTeleport :: [String] -> Either String Command
parseTeleport [xs, ys] = case (readMaybe xs, readMaybe ys) of
  (Just x, Just y) -> Right (CmdTeleport (V2 x y))
  _                -> Left "usage: tp X Y (X and Y must be integers)"
parseTeleport _ = Left "usage: tp X Y"

parseSpawn :: [String] -> Either String Command
parseSpawn [k] = case map toLower k of
  "rat"    -> Right (CmdSpawn Rat)
  "goblin" -> Right (CmdSpawn Goblin)
  "orc"    -> Right (CmdSpawn Orc)
  other    -> Left ("unknown monster kind: " ++ other)
parseSpawn _ = Left "usage: spawn <rat|goblin|orc>"

parseXP :: [String] -> Either String Command
parseXP [n] = case readMaybe n of
  Just i | i >= 0 -> Right (CmdXP i)
  _               -> Left "usage: xp N (N must be a non-negative integer)"
parseXP _ = Left "usage: xp N"
