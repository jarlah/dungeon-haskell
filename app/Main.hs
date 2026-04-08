module Main (main) where

import Brick
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import System.Random (newStdGen)

import Game.GameState
import Game.Input (handleKey)
import Game.Logic.Dungeon (defaultLevelConfig, generateLevel)
import Game.Render (drawGame)
import Game.Types (GameAction(..))

app :: App GameState e ()
app = App
  { appDraw         = drawGame
  , appChooseCursor = showFirstCursor
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
  gen <- newStdGen
  let (dl, startPos, _) = generateLevel gen defaultLevelConfig
      initialState      = mkGameState dl startPos
      buildVty          = VCP.mkVty V.defaultConfig
  initialVty <- buildVty
  _ <- customMain initialVty buildVty Nothing app initialState
  pure ()
