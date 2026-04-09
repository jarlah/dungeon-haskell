-- | Tests for the pure helpers in "Game.UI.Prompt". The 'EventM'
--   handlers thread live Brick state and can't be driven from a
--   spec, but the two pure state machines — the prompt-buffer
--   stepper and the command router — carry all the interesting
--   branching. These tests pin them down.
module Game.UI.PromptSpec (spec) where

import qualified Graphics.Vty as V
import Test.Hspec

import Game.Logic.Command (Command (..))
import Game.Types (MonsterKind (..))
import Game.UI.Prompt
  ( CommandEffect (..), PromptStep (..)
  , routeCommand, stepPromptBuffer
  )
import Game.UI.Types (RuntimeFlags (..))
import Game.GameState (SaveMenuMode (..))
import Linear (V2 (..))

wizard, clean :: RuntimeFlags
wizard = RuntimeFlags { rfWizardEnabled = True }
clean  = RuntimeFlags { rfWizardEnabled = False }

spec :: Spec
spec = do

  describe "stepPromptBuffer" $ do

    it "Esc cancels regardless of buffer contents" $ do
      stepPromptBuffer V.KEsc ""      `shouldBe` PromptCancel
      stepPromptBuffer V.KEsc "help"  `shouldBe` PromptCancel

    it "Enter submits the current buffer verbatim" $ do
      stepPromptBuffer V.KEnter "help" `shouldBe` PromptSubmit "help"
      stepPromptBuffer V.KEnter ""     `shouldBe` PromptSubmit ""

    it "Backspace drops the last character" $ do
      stepPromptBuffer V.KBS "help" `shouldBe` PromptEdit "hel"
      stepPromptBuffer V.KBS "h"    `shouldBe` PromptEdit ""

    it "Backspace on an empty buffer is a no-op edit" $
      stepPromptBuffer V.KBS "" `shouldBe` PromptEdit ""

    it "printable characters append to the buffer" $ do
      stepPromptBuffer (V.KChar 'h') ""    `shouldBe` PromptEdit "h"
      stepPromptBuffer (V.KChar 'p') "hel" `shouldBe` PromptEdit "help"

    it "other keys are ignored" $ do
      stepPromptBuffer V.KUp      "help" `shouldBe` PromptIgnore
      stepPromptBuffer V.KDown    "help" `shouldBe` PromptIgnore
      stepPromptBuffer (V.KFun 5) "help" `shouldBe` PromptIgnore

  describe "routeCommand: safe commands route the same way in both modes" $ do

    let safeCases =
          [ (CmdHelp,      EffSetHelp)
          , (CmdQuit,      EffConfirmQuit)
          , (CmdInventory, EffSetInventory)
          , (CmdQuests,    EffSetQuestLog)
          , (CmdWait,      EffApplyWait)
          , (CmdSave,      EffSaveMenu SaveMode)
          , (CmdLoad,      EffSaveMenu LoadMode)
          , (CmdQuicksave, EffQuicksave)
          , (CmdQuickload, EffQuickload)
          ]

    mapM_
      (\(cmd, eff) -> it (show cmd ++ " -> " ++ show eff) $ do
         routeCommand wizard cmd `shouldBe` eff
         routeCommand clean  cmd `shouldBe` eff)
      safeCases

  describe "routeCommand: cheat commands" $ do

    let cheatCommands =
          [ CmdReveal
          , CmdHeal
          , CmdKillAll
          , CmdTeleport (V2 1 1)
          , CmdSpawn Rat
          , CmdXP 10
          , CmdDescend
          , CmdAscend
          ]

    it "pass through to EffApplyCommand when wizard mode is on" $ do
      mapM_
        (\c -> routeCommand wizard c `shouldBe` EffApplyCommand c)
        cheatCommands

    it "are blocked when wizard mode is off" $ do
      mapM_
        (\c -> routeCommand clean c `shouldBe` EffCheatBlocked)
        cheatCommands
