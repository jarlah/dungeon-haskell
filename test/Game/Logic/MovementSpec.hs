{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Game.Logic.MovementSpec (spec) where

import qualified Data.Vector as V
import Linear (V2(..))
import Test.Hspec
import Test.QuickCheck

import Game.Logic.Movement
import Game.Types

-- | A 10x10 room: walls around the border, floor inside.
testRoom :: DungeonLevel
testRoom = DungeonLevel
  { dlWidth  = 10
  , dlHeight = 10
  , dlDepth  = 1
  , dlTiles  = V.generate (10 * 10) mkTile
  }
  where
    mkTile i =
      let (y, x) = i `divMod` 10
      in if x == 0 || y == 0 || x == 9 || y == 9 then Wall else Floor

instance Arbitrary Dir where
  arbitrary = elements [minBound .. maxBound]

-- | A position strictly inside the test room interior (walkable floor).
newtype InteriorPos = InteriorPos Pos deriving (Eq, Show)

instance Arbitrary InteriorPos where
  arbitrary = do
    x <- chooseInt (1, dlWidth testRoom - 2)
    y <- chooseInt (1, dlHeight testRoom - 2)
    pure $ InteriorPos (V2 x y)

invertDir :: Dir -> Dir
invertDir N  = S
invertDir NE = SW
invertDir E  = W
invertDir SE = NW
invertDir S  = N
invertDir SW = NE
invertDir W  = E
invertDir NW = SE

spec :: Spec
spec = describe "Game.Logic.Movement.tryMove" $ do

  it "prop_cantWalkThroughWalls" $ property $
    \(InteriorPos p) dir ->
      let target = p + dirToOffset dir
      in case tileAt testRoom target of
           Just Wall -> tryMove testRoom p dir === Nothing
           _         -> property True

  it "prop_moveAndBackReturnsToStart" $ property $
    \(InteriorPos p) dir ->
      case tryMove testRoom p dir of
        Nothing -> property True
        Just p' -> tryMove testRoom p' (invertDir dir) === Just p

  it "prop_allDirectionsMoveExactlyOneStep" $ property $
    \(InteriorPos p) dir ->
      case tryMove testRoom p dir of
        Nothing -> property True
        Just p' ->
          let V2 dx dy = p' - p
          in counterexample (show (dx, dy)) $
               abs dx <= 1 && abs dy <= 1 && (dx, dy) /= (0, 0)

  it "prop_stayInBounds" $ property $
    \(InteriorPos p) dir ->
      case tryMove testRoom p dir of
        Nothing       -> property True
        Just (V2 x y) ->
          property $
            x >= 0 && x < dlWidth testRoom
              && y >= 0 && y < dlHeight testRoom
