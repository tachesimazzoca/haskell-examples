{-
  * Reverse

    ghci> reverse' [1, 2, 3, 4]
    -- (reverse' [2, 3, 4]) ++ [1]
    -- ((reverse' [3, 4]) ++ [2]) ++ [1]
    -- (((reverse' [4]) ++ [3]) ++ [2]) ++ [1]
    -- ((((reverse' []) ++ [4]) ++ [3]) ++ [2]) ++ [1]
    -- (((([]) ++ [4]) ++ [3]) ++ [2]) ++ [1]
    [4, 3, 2, 1]
-}

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
