module Game.Input (handleKey) where

import qualified Graphics.Vty as V

import Game.Types

-- | Translate a Vty key press into a 'GameAction', if any.
handleKey :: V.Key -> [V.Modifier] -> Maybe GameAction
handleKey key _mods = case key of
  V.KUp       -> Just (Move N)
  V.KDown     -> Just (Move S)
  V.KLeft     -> Just (Move W)
  V.KRight    -> Just (Move E)
  V.KChar 'k' -> Just (Move N)
  V.KChar 'j' -> Just (Move S)
  V.KChar 'h' -> Just (Move W)
  V.KChar 'l' -> Just (Move E)
  V.KChar 'y' -> Just (Move NW)
  V.KChar 'u' -> Just (Move NE)
  V.KChar 'b' -> Just (Move SW)
  V.KChar 'n' -> Just (Move SE)
  V.KChar '.' -> Just Wait
  V.KChar 'g' -> Just Pickup
  V.KChar 'q' -> Just Quit
  V.KEsc      -> Just Quit
  _           -> Nothing
