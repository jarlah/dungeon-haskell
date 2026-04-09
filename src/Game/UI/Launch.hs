-- | Launch / title-screen handlers, extracted from @app/Main.hs@.
--
--   These handlers drive the layer that sits between @main@ and the
--   gameplay loop: navigating the launch menu, committing a choice
--   (New Game / Continue / Load / Quit), and surfacing load errors
--   without abandoning the menu. Everything here talks to the save
--   system through 'Game.UI.SaveMenu' so the filter / listing
--   policy stays in a single place.
module Game.UI.Launch
  ( handleLaunchMenuKey
  , runLaunchOption
  , loadMostRecent
  , launchError
  ) where

import Brick (EventM, halt, modify, put)
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V

import Game.GameState
import qualified Game.Save as Save
import Game.Save.Types (SaveMetadata (..))
import Game.UI.SaveMenu (filterLoadable, openSaveMenu, showSaveError)
import Game.UI.Types (Name, RuntimeFlags)

-- | Keystrokes while the launch screen is up. Only navigation,
--   selection, and quit are meaningful — everything else is
--   swallowed so a fat-fingered key doesn't leak into the game.
handleLaunchMenuKey
  :: RuntimeFlags
  -> LaunchMenu
  -> V.Key
  -> EventM Name GameState ()
handleLaunchMenuKey _  lm V.KUp =
  let n = length launchOptions
      c = (lmCursor lm - 1) `mod` n
  in modify $ \gs -> gs { gsLaunchMenu = Just lm { lmCursor = c } }
handleLaunchMenuKey _  lm V.KDown =
  let n = length launchOptions
      c = (lmCursor lm + 1) `mod` n
  in modify $ \gs -> gs { gsLaunchMenu = Just lm { lmCursor = c } }
handleLaunchMenuKey rFlags lm V.KEnter = runLaunchOption rFlags lm
handleLaunchMenuKey _  _  (V.KChar 'q') = halt
handleLaunchMenuKey _  _  (V.KChar 'Q') = halt
handleLaunchMenuKey _  _  V.KEsc        = halt
-- Letter shortcuts: n/c/l/q match the first letter of each option
-- so a keyboard player doesn't have to arrow-navigate.
handleLaunchMenuKey rFlags lm (V.KChar 'n') =
  runLaunchOption rFlags lm { lmCursor = 0 } -- New Game
handleLaunchMenuKey rFlags lm (V.KChar 'c')
  | lmHasSaves lm = runLaunchOption rFlags lm { lmCursor = 1 } -- Continue
handleLaunchMenuKey rFlags lm (V.KChar 'l')
  | lmHasSaves lm = runLaunchOption rFlags lm { lmCursor = 2 } -- Load
handleLaunchMenuKey _ _ _ = pure ()

-- | Execute the option currently highlighted by 'lmCursor'.
--   Disabled options (Continue / Load with no saves) are silently
--   ignored so the UI doesn't need a separate "error" state.
runLaunchOption :: RuntimeFlags -> LaunchMenu -> EventM Name GameState ()
runLaunchOption rFlags lm = case drop (lmCursor lm) launchOptions of
  []        -> pure ()
  (opt : _) -> case opt of
    LaunchNewGame ->
      -- The state we started with is already a fresh run — just
      -- drop the launch menu and let the game take over.
      modify $ \gs -> gs { gsLaunchMenu = Nothing }
    LaunchContinue
      | not (lmHasSaves lm) -> pure ()
      | otherwise           -> loadMostRecent rFlags
    LaunchLoad
      | not (lmHasSaves lm) -> pure ()
      | otherwise           -> do
          -- Clear the launch menu first so the save picker layers
          -- on top of the gameplay screen the way it does mid-run.
          modify $ \gs -> gs { gsLaunchMenu = Nothing }
          openSaveMenu rFlags LoadMode
    LaunchQuit -> halt

-- | Load the most recently modified save visible to the current
--   session. Cheat-tainted saves are filtered out in non-wizard
--   mode so a clean session can't accidentally continue a hacked
--   run. 'Save.listSaves' already sorts newest-first, so we just
--   take the head of the filtered list. On any failure we surface
--   a message and leave the launch menu open so the player can
--   pick again.
loadMostRecent :: RuntimeFlags -> EventM Name GameState ()
loadMostRecent rFlags = do
  listRes <- liftIO Save.listSaves
  case listRes of
    Left err ->
      launchError ("Couldn't list saves: " ++ showSaveError err)
    Right ms -> case filterLoadable rFlags ms of
      []       ->
        launchError "No saves to continue from."
      (md : _) -> do
        readRes <- liftIO (Save.readSave (smSlot md))
        case readRes of
          Left err ->
            launchError ("Continue failed: " ++ showSaveError err)
          Right loaded ->
            -- Loaded state drops the launch menu automatically
            -- because 'performSaveAt' wiped it before the blob was
            -- written. Prepend a breadcrumb so the player sees the
            -- load happened in the message log.
            put loaded
              { gsLaunchMenu = Nothing
              , gsMessages   = "Continued from most recent save."
                               : gsMessages loaded
              }

-- | Push a one-line error into 'gsMessages' without closing the
--   launch menu. Used when 'Continue' can't find or can't read the
--   most recent save.
launchError :: String -> EventM Name GameState ()
launchError msg =
  modify $ \gs -> gs { gsMessages = msg : gsMessages gs }
