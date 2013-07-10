{-
  * Exists

    ghci> 2 `exists` [1, 2, 3]
    2 == 1 || 2 `exists` [2, 3]
    2 == 2 || 2 `exists` [3]
    True
-}

exists :: (Eq a) => a -> [a] -> Bool
exists _ [] = False
exists p (x:xs)
  | p == x = True
  | otherwise = p `exists` xs
