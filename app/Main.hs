module Main (main) where

import Brick
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import System.Random (newStdGen)

import qualified Game.Audio as Audio
import Game.GameState
import Game.Input (handleKey)
import Game.Logic.Dungeon (defaultLevelConfig)
import Game.Render (drawGame, fogAttr)
import Game.Types (GameAction(..))

-- | Build the Brick 'App' with audio closed into the event handler.
--   Passing 'Nothing' disables audio playback (silent run).
mkApp :: Maybe Audio.AudioSystem -> App GameState e ()
mkApp mAudio = App
  { appDraw         = drawGame
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = handleEvent mAudio
  , appStartEvent   = pure ()
  , appAttrMap      = const $ attrMap V.defAttr
      [ (fogAttr, fg V.brightBlack)
      ]
  }

handleEvent :: Maybe Audio.AudioSystem -> BrickEvent () e -> EventM () GameState ()
handleEvent mAudio (VtyEvent (V.EvKey key mods)) =
  case handleKey key mods of
    Just Quit -> halt
    Just act  -> do
      modify (applyAction act)
      case mAudio of
        Nothing    -> pure ()
        Just audio -> do
          gs <- get
          liftIO $ mapM_ (Audio.playEvent audio) (gsEvents gs)
    Nothing   -> pure ()
handleEvent _ _ = pure ()

main :: IO ()
main = do
  gen <- newStdGen
  let initialState = newGame gen defaultLevelConfig
      buildVty     = VCP.mkVty V.defaultConfig
  -- Audio init is best-effort: if it fails (no device, missing
  -- assets, ...), 'bracket' still runs the game silently.
  bracket Audio.initAudio
          (mapM_ Audio.shutdownAudio)
          $ \mAudio -> do
    initialVty <- buildVty
    _ <- customMain initialVty buildVty Nothing (mkApp mAudio) initialState
    pure ()
