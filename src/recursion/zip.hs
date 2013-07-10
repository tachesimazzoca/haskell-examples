{-
  * Zip

    ghci> zip' [1..4] ["One", "Two", "Three"]
    -- (1, "One") : zip' [2, 3, 4] ["Two", "Three"]
    -- (1, "One") : ((2, "Two") : zip' [3, 4] ["Three"])
    -- (1, "One") : ((2, "Two") : ((3, "Three") : zip' [4] []))
    -- (1, "One") : ((2, "Two") : ((3, "Three") : []))
    [(1, "One"), (2, "Two"), (3, "Three")]
-}

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
