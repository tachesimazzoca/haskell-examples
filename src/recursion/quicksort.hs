{-
  * Quick Sort

    ghci> quicksort [5, 1, 9, 4, 6, 7 ,3]
    -- [1, 4, 3] ++ [5] ++ [9, 6, 7]
    -- ([] ++ [1] ++ [4, 3]) ++ [5] ++ ([6, 7] ++ [9] ++ [])
    -- ([1] ++ ([] ++ [3] ++ [4])) ++ [5] ++ (([] ++ [6] ++ [7]) ++ 9 ++ [])
    [1, 3, 4, 5, 6, 7, 9]
-}

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
  where smaller = [p | p <- xs, p <= x]
        larger = [p | p <- xs, p > x]
