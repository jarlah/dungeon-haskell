-- | Monster loot tables. Given a slain monster's kind and a RNG,
--   roll zero or more items to drop at its feet.
--
--   The tables are deliberately simple — a fixed drop chance per
--   kind plus a weighted pick among a small candidate list. Both
--   pieces are pure data at the top of the module so they're easy
--   to tweak and test.
module Game.Logic.Loot
  ( rollLoot
  , dropChance
  , lootTable
  ) where

import System.Random (StdGen, randomR)

import Game.Types

-- | Probability that a given monster kind drops anything at all,
--   expressed as a threshold: the roll is @[1, 100]@ and a drop
--   happens when the roll is @≤ dropChance kind@.
dropChance :: MonsterKind -> Int
dropChance Rat    = 20
dropChance Goblin = 45
dropChance Orc    = 75

-- | The weighted candidate list for a given monster kind. Weights
--   don't have to sum to anything specific; 'rollLoot' picks
--   uniformly in @[1, totalWeight]@ and walks the list.
--
--   Rats mostly cough up scraps — a minor potion now and then.
--   Goblins start carrying short swords. Orcs are the only reliable
--   source of real armor and major potions.
lootTable :: MonsterKind -> [(Int, Item)]
lootTable Rat =
  [ (3, IPotion HealingMinor)
  ]
lootTable Goblin =
  [ (4, IPotion HealingMinor)
  , (1, IPotion HealingMajor)
  , (2, IWeapon ShortSword)
  , (1, IArmor  LeatherArmor)
  ]
lootTable Orc =
  [ (3, IPotion HealingMinor)
  , (3, IPotion HealingMajor)
  , (2, IWeapon ShortSword)
  , (2, IWeapon LongSword)
  , (2, IArmor  LeatherArmor)
  , (1, IArmor  ChainMail)
  ]

-- | Roll loot for a freshly killed monster. Returns a (possibly
--   empty) list of items plus the advanced generator. Never drops
--   more than one item per kill — keeping drops sparse so
--   inventory fills up through play rather than in ten turns.
rollLoot :: StdGen -> MonsterKind -> ([Item], StdGen)
rollLoot gen0 kind =
  let (roll, gen1) = randomR (1 :: Int, 100) gen0
  in if roll > dropChance kind
       then ([], gen1)
       else case pickWeighted gen1 (lootTable kind) of
              Nothing           -> ([],  gen1)
              Just (item, gen2) -> ([item], gen2)

-- | Weighted pick from a @[(weight, a)]@ list. 'Nothing' iff the
--   list is empty (or every weight is non-positive).
pickWeighted :: StdGen -> [(Int, a)] -> Maybe (a, StdGen)
pickWeighted _    []    = Nothing
pickWeighted gen0 table =
  let total = sum (map fst table)
  in if total <= 0
       then Nothing
       else
         let (roll, gen1) = randomR (1, total) gen0
         in Just (walk roll table, gen1)
  where
    walk _ []                        = error "pickWeighted: empty walk"
    walk n ((w, x) : rest)
      | n <= w    = x
      | otherwise = walk (n - w) rest
