module Game.GameState
  ( GameState(..)
  , initialGameState
  , hardcodedRoom
  , applyAction
  ) where

import qualified Data.Vector as V
import Linear (V2(..))

import Game.Types
import qualified Game.Logic.Movement as M

-- | Pure snapshot of the whole game world for Milestone 1.
--
--   Later milestones will swap this out for an Apecs world once the
--   number of entities justifies the ECS overhead.
data GameState = GameState
  { gsLevel     :: !DungeonLevel
  , gsPlayerPos :: !Pos
  , gsQuitting  :: !Bool
  } deriving (Eq, Show)

-- | A hardcoded 20x10 room: walls around the border, floor inside.
hardcodedRoom :: DungeonLevel
hardcodedRoom = DungeonLevel
  { dlWidth  = 20
  , dlHeight = 10
  , dlDepth  = 1
  , dlTiles  = V.generate (20 * 10) mkTile
  }
  where
    mkTile i =
      let (y, x) = i `divMod` 20
      in if x == 0 || y == 0 || x == 19 || y == 9
           then Wall
           else Floor

initialGameState :: GameState
initialGameState = GameState
  { gsLevel     = hardcodedRoom
  , gsPlayerPos = V2 5 5
  , gsQuitting  = False
  }

-- | Apply an action to the game state. Pure; no IO.
applyAction :: GameAction -> GameState -> GameState
applyAction act gs = case act of
  Quit     -> gs { gsQuitting = True }
  Wait     -> gs
  Move dir ->
    case M.tryMove (gsLevel gs) (gsPlayerPos gs) dir of
      Just newPos -> gs { gsPlayerPos = newPos }
      Nothing     -> gs
