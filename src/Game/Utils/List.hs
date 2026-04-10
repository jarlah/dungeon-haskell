module Game.Utils.List(
 updateAt
,removeAt
,findIndexed
,safeIndex
) where

------------------------------------------------------------
-- List helpers
------------------------------------------------------------

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f xs
  | i < 0 || i >= length xs = xs
  | otherwise = take i xs ++ [f (xs !! i)] ++ drop (i + 1) xs

-- | Safe list indexing — returns 'Nothing' for out-of-bounds indices.
--   Single traversal: O(n) instead of O(2n).
safeIndex :: Int -> [a] -> Maybe a
safeIndex n _ | n < 0 = Nothing
safeIndex 0 (x:_)     = Just x
safeIndex n (_:xs)     = safeIndex (n-1) xs
safeIndex _ []         = Nothing

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i + 1) xs

-- | Return the first element (and its zero-based index) that
--   matches the predicate, or Nothing.
findIndexed :: (a -> Bool) -> [a] -> Maybe (Int, a)
findIndexed p = go 0
  where
    go _ [] = Nothing
    go i (x : rest)
      | p x       = Just (i, x)
      | otherwise = go (i + 1) rest