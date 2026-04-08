module Main (main) where

import Brick
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP

import Game.GameState
import Game.Input (handleKey)
import Game.Render (drawGame)
import Game.Types (GameAction(..))

app :: App GameState e ()
app = App
  { appDraw         = drawGame
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const $ attrMap V.defAttr []
  }

handleEvent :: BrickEvent () e -> EventM () GameState ()
handleEvent (VtyEvent (V.EvKey key mods)) =
  case handleKey key mods of
    Just Quit -> halt
    Just act  -> modify (applyAction act)
    Nothing   -> pure ()
handleEvent _ = pure ()

main :: IO ()
main = do
  let buildVty = VCP.mkVty V.defaultConfig
  initialVty <- buildVty
  _ <- customMain initialVty buildVty Nothing app initialGameState
  pure ()
