{-
  * Drop

    ghci> drop' 3 [1, 2, 3, 4]
    -- drop' 2 [2, 3, 4]
    -- drop' 1 [3, 4]
    -- drop' 0 [4]
    [4]
-}

drop' :: Int -> [a] -> [a]
drop' n xs
  | n <= 0 = xs
drop' _ [] = []
drop' n (x:xs) = drop' (n - 1) xs
