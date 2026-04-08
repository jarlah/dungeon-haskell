{-# LANGUAGE ScopedTypeVariables #-}
-- | Tests for the semantic-event list that 'applyAction' emits.
--
--   These are deterministic: we construct tiny fixture 'GameState's
--   with hand-picked RNG + stats so the combat outcome is forced.
module Game.GameStateSpec (spec) where

import qualified Data.Set as Set
import qualified Data.Vector as V
import Linear (V2(..))
import System.Random (mkStdGen)
import Test.Hspec

import Game.GameState
import Game.Types

-- | A tiny 5x5 open room with walls around the edge, so the player
--   has somewhere to move and monsters have space to act.
tinyRoom :: DungeonLevel
tinyRoom = DungeonLevel
  { dlWidth  = 5
  , dlHeight = 5
  , dlDepth  = 1
  , dlTiles  = V.generate (5 * 5) $ \i ->
      let (y, x) = i `divMod` 5
      in if x == 0 || y == 0 || x == 4 || y == 4 then Wall else Floor
  }

-- | Build a 'GameState' with the player at a chosen spot, given
--   stats, and a known RNG seed.
mkFixture :: Int -> Pos -> Stats -> [Monster] -> GameState
mkFixture seed ppos pstats monsters = GameState
  { gsLevel       = tinyRoom
  , gsPlayerPos   = ppos
  , gsPlayerStats = pstats
  , gsMonsters    = monsters
  , gsMessages    = []
  , gsRng         = mkStdGen seed
  , gsDead        = False
  , gsQuitting    = False
  , gsEvents      = [EvAttackHit]  -- stale, should be cleared by applyAction
  , gsVisible     = Set.empty       -- applyAction recomputes these
  , gsExplored    = Set.empty
  }

-- | Player stats strong enough to one-shot anything normal.
overpoweredPlayer :: Stats
overpoweredPlayer = Stats
  { sHP = 20, sMaxHP = 20
  , sAttack = 9999, sDefense = 99
  , sSpeed = 10
  , sLevel = 1, sXP = 0
  }

-- | A rat placed at (2, 1), one tile north of a player at (2, 2).
ratAt :: Pos -> Monster
ratAt p = Monster
  { mKind  = Rat
  , mPos   = p
  , mStats = monsterStats Rat
  }

spec :: Spec
spec = describe "Game.GameState.applyAction / event emission" $ do

  it "a blocked move produces no events and clears any stale ones" $ do
    -- Player at (1,1); walking N would hit the wall at (1,0).
    let gs  = mkFixture 42 (V2 1 1) overpoweredPlayer []
        gs' = applyAction (Move N) gs
    gsEvents gs' `shouldBe` []

  it "Wait on an empty level produces no events" $ do
    let gs  = mkFixture 1 (V2 2 2) overpoweredPlayer []
        gs' = applyAction Wait gs
    gsEvents gs' `shouldBe` []

  it "an unobstructed move produces no events" $ do
    -- Player at (2,2), moving E into an empty floor tile.
    let gs  = mkFixture 7 (V2 2 2) overpoweredPlayer []
        gs' = applyAction (Move E) gs
    gsEvents gs' `shouldBe` []

  it "killing a rat emits EvMonsterKilled" $ do
    -- Player at (2,2), rat at (2,1). Move N bumps the rat. Seed 3 is
    -- known to roll a Kill against a rat for this stat block (verified
    -- in GHCi — hit threshold bottoms at 5 so we can't assume any seed
    -- works; we pick one that does).
    let rat = ratAt (V2 2 1)
        gs  = mkFixture 3 (V2 2 2) overpoweredPlayer [rat]
        gs' = applyAction (Move N) gs
    EvMonsterKilled `elem` gsEvents gs' `shouldBe` True

  it "the rat is removed from the level after the killing blow" $ do
    let rat = ratAt (V2 2 1)
        gs  = mkFixture 3 (V2 2 2) overpoweredPlayer [rat]
        gs' = applyAction (Move N) gs
    length (gsMonsters gs') `shouldBe` 0

  it "killing a monster can fire EvLevelUp when enough XP is gained" $ do
    -- Put the player one XP away from level 2 and kill a rat (5 XP).
    let almostLeveled = overpoweredPlayer { sXP = 24 }   -- curve: 25 -> 2
        rat           = ratAt (V2 2 1)
        gs            = mkFixture 3 (V2 2 2) almostLeveled [rat]
        gs'           = applyAction (Move N) gs
    EvLevelUp `elem` gsEvents gs' `shouldBe` True

  it "events are fresh each turn (stale events don't leak)" $ do
    -- The fixture seeds a stale EvAttackHit; an empty-tile move must
    -- not preserve it.
    let gs  = mkFixture 99 (V2 2 2) overpoweredPlayer []
        gs' = applyAction Wait gs
    EvAttackHit `elem` gsEvents gs' `shouldBe` False
