{-
  * Maximum

    ghci> maximum' [2, 5, 1]
    -- max 2 (maximum' [5, 1])
    -- max 2 (max 5 (maximum' [1]))
    -- max 2 (max 5 1)
    -- max 2 5
    ghci> 5
-}

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
