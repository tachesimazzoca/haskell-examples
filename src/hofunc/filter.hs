{-
  * Filter

    ghci> filter' even [1..10]
    -- (filter` even [2..10])
    -- 2 : (filter` even [3..10])
    -- 2 : (filter` even [4..10])
    -- 2 : (4 : (filter` even [5..10]))
    -- ..
    [2, 4, 6, 8, 10]
    ghci> let notBlank x = not (x `elem` [' ']) in filter' notBlank "DEAD BEEF"
    "DEADBEEF"
-}

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x : (filter' f xs)
  | otherwise = filter' f xs
