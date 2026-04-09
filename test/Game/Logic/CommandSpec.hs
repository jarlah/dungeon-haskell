-- | Tests for the slash-command parser. Exercise both the happy
--   path (every command the parser understands) and the error
--   path (bad arguments, unknown verbs, stray tokens on nullary
--   commands).
module Game.Logic.CommandSpec (spec) where

import Linear (V2(..))
import Test.Hspec

import Game.Logic.Command
import Game.Types (MonsterKind(..))

spec :: Spec
spec = describe "Game.Logic.Command.parseCommand" $ do

  describe "reveal" $ do
    it "parses 'reveal'" $
      parseCommand "reveal" `shouldBe` Right CmdReveal

    it "parses '/reveal' with a leading slash" $
      parseCommand "/reveal" `shouldBe` Right CmdReveal

    it "tolerates leading whitespace" $
      parseCommand "   reveal" `shouldBe` Right CmdReveal

    it "is case-insensitive on the verb" $ do
      parseCommand "REVEAL" `shouldBe` Right CmdReveal
      parseCommand "Reveal" `shouldBe` Right CmdReveal

    it "rejects stray arguments on reveal" $
      parseCommand "reveal foo" `shouldBe`
        Left "command takes no arguments, got: foo"

  describe "nullary wizard commands" $ do
    it "parses heal"       $ parseCommand "heal"     `shouldBe` Right CmdHeal
    it "parses kill-all"   $ parseCommand "kill-all" `shouldBe` Right CmdKillAll
    it "parses killall"    $ parseCommand "killall"  `shouldBe` Right CmdKillAll
    it "parses descend"    $ parseCommand "descend"  `shouldBe` Right CmdDescend
    it "parses ascend"     $ parseCommand "ascend"   `shouldBe` Right CmdAscend
    it "accepts /heal with leading slash" $
      parseCommand "/heal" `shouldBe` Right CmdHeal

  describe "teleport" $ do
    it "parses 'tp 5 7'" $
      parseCommand "tp 5 7" `shouldBe` Right (CmdTeleport (V2 5 7))

    it "parses the full spelling 'teleport 1 2'" $
      parseCommand "teleport 1 2" `shouldBe` Right (CmdTeleport (V2 1 2))

    it "accepts negative coordinates (the handler filters unreachable targets)" $
      parseCommand "tp -1 -2" `shouldBe` Right (CmdTeleport (V2 (-1) (-2)))

    it "rejects missing arguments" $
      parseCommand "tp 5" `shouldBe` Left "usage: tp X Y"

    it "rejects non-integer coordinates" $
      parseCommand "tp a b" `shouldBe`
        Left "usage: tp X Y (X and Y must be integers)"

  describe "spawn" $ do
    it "parses rat / goblin / orc case-insensitively" $ do
      parseCommand "spawn rat"    `shouldBe` Right (CmdSpawn Rat)
      parseCommand "spawn GOBLIN" `shouldBe` Right (CmdSpawn Goblin)
      parseCommand "spawn Orc"    `shouldBe` Right (CmdSpawn Orc)

    it "rejects unknown monster kinds" $
      parseCommand "spawn dragon" `shouldBe`
        Left "unknown monster kind: dragon"

    it "requires exactly one argument" $ do
      parseCommand "spawn"         `shouldBe` Left "usage: spawn <rat|goblin|orc>"
      parseCommand "spawn rat orc" `shouldBe` Left "usage: spawn <rat|goblin|orc>"

  describe "xp" $ do
    it "parses 'xp 50'" $
      parseCommand "xp 50" `shouldBe` Right (CmdXP 50)

    it "accepts zero" $
      parseCommand "xp 0" `shouldBe` Right (CmdXP 0)

    it "rejects negative amounts" $
      parseCommand "xp -5" `shouldBe`
        Left "usage: xp N (N must be a non-negative integer)"

    it "rejects non-integer amounts" $
      parseCommand "xp lots" `shouldBe`
        Left "usage: xp N (N must be a non-negative integer)"

    it "rejects missing argument" $
      parseCommand "xp" `shouldBe` Left "usage: xp N"

  describe "error cases" $ do
    it "rejects the empty buffer" $
      parseCommand "" `shouldBe` Left "empty command"

    it "rejects an unknown command with a helpful message" $
      parseCommand "fly" `shouldBe` Left "unknown command: fly"

  -- ----------------------------------------------------------------
  -- Safe UI commands — always available, even without --wizard.
  -- ----------------------------------------------------------------

  describe "safe UI commands" $ do
    it "parses /help"       $ parseCommand "/help"      `shouldBe` Right CmdHelp
    it "parses /quit"       $ parseCommand "/quit"      `shouldBe` Right CmdQuit
    it "parses /exit as quit" $ parseCommand "/exit"    `shouldBe` Right CmdQuit
    it "parses /inv"        $ parseCommand "/inv"       `shouldBe` Right CmdInventory
    it "parses /inventory"  $ parseCommand "/inventory" `shouldBe` Right CmdInventory
    it "parses /quests"     $ parseCommand "/quests"    `shouldBe` Right CmdQuests
    it "parses /questlog"   $ parseCommand "/questlog"  `shouldBe` Right CmdQuests
    it "parses /wait"       $ parseCommand "/wait"      `shouldBe` Right CmdWait
    it "parses /rest as wait" $ parseCommand "/rest"    `shouldBe` Right CmdWait
    it "parses /save"       $ parseCommand "/save"      `shouldBe` Right CmdSave
    it "parses /load"       $ parseCommand "/load"      `shouldBe` Right CmdLoad
    it "parses /quicksave"  $ parseCommand "/quicksave" `shouldBe` Right CmdQuicksave
    it "parses /qs alias"   $ parseCommand "/qs"        `shouldBe` Right CmdQuicksave
    it "parses /quickload"  $ parseCommand "/quickload" `shouldBe` Right CmdQuickload
    it "parses /ql alias"   $ parseCommand "/ql"        `shouldBe` Right CmdQuickload

    it "is case-insensitive on safe verbs" $
      parseCommand "/HELP" `shouldBe` Right CmdHelp

    it "rejects stray args on safe nullary commands" $
      parseCommand "/help me" `shouldBe`
        Left "command takes no arguments, got: me"

  -- ----------------------------------------------------------------
  -- Cheat classification — drives the capability check in Main.
  -- ----------------------------------------------------------------

  describe "isCheatCommand" $ do
    it "tags all wizard commands as cheats" $ do
      isCheatCommand CmdReveal                   `shouldBe` True
      isCheatCommand CmdHeal                     `shouldBe` True
      isCheatCommand CmdKillAll                  `shouldBe` True
      isCheatCommand (CmdTeleport (V2 0 0))      `shouldBe` True
      isCheatCommand (CmdSpawn Rat)              `shouldBe` True
      isCheatCommand (CmdXP 1)                   `shouldBe` True
      isCheatCommand CmdDescend                  `shouldBe` True
      isCheatCommand CmdAscend                   `shouldBe` True

    it "tags all safe UI commands as non-cheats" $ do
      isCheatCommand CmdHelp       `shouldBe` False
      isCheatCommand CmdQuit       `shouldBe` False
      isCheatCommand CmdInventory  `shouldBe` False
      isCheatCommand CmdQuests     `shouldBe` False
      isCheatCommand CmdWait       `shouldBe` False
      isCheatCommand CmdSave       `shouldBe` False
      isCheatCommand CmdLoad       `shouldBe` False
      isCheatCommand CmdQuicksave  `shouldBe` False
      isCheatCommand CmdQuickload  `shouldBe` False
