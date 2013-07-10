{-
  * Unique

    ghci> unique [1, 2, 2, 3]
    -- 1 : unique [p | p <- [2, 2, 3], p /= 1]
    -- 1 : unique [2, 2, 3]
    -- 1 : (2 : unique [p | p <- [2, 3], p /= 2])
    -- 1 : (2 : unique [3])
    -- 1 : (2 : (3 : unique [p | p <- [], p /= 3]))
    -- 1 : (2 : (3 : unique []))
    -- 1 : (2 : (3 : []))
    [1, 2, 3]
-}

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs) = x : unique [p | p <- xs, p /= x]
