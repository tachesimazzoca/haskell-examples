{-
  * Take

    ghci> take' 3 [1, 2, 3, 4]
    -- 1 : (take' 2 [2, 3, 4])
    -- 1 : (2 : (take' 1 [3, 4]))
    -- 1 : (2 : (3 : (take' 0 [4])))
    -- 1 : (2 : (3 : []))
    [1, 2, 3]
-}

take' :: Int -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs
