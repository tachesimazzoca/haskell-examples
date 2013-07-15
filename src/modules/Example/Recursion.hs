module Example.Recursion where

-- | Collatz conjecture <http://en.wikipedia.org/wiki/Collatz_conjecture>
--
-- >>> collatz 10
-- [10,5,16,8,4,2,1]
--
-- > 10 : collatz 5
-- > 10 : (5 : collatz 16)
-- > 10 : (5 : (16 : collatz 8))
-- > 10 : (5 : (16 : (8 : collatz 4)))
-- > 10 : (5 : (16 : (8 : (4 : collatz 2))))
-- > 10 : (5 : (16 : (8 : (4 : (2 : collatz 1)))))
-- > 10 : (5 : (16 : (8 : (4 : (2 : [1])))))
collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz x
  | even x    = x : collatz (x `div` 2)
  | otherwise = x : collatz (x * 3 + 1)

-- |
--
-- >>> drop' 3 [1, 2, 3, 4]
-- [4]
--
-- > drop' 2 [2, 3, 4]
-- > drop' 1 [3, 4]
-- > drop' 0 [4]
drop' :: Int -> [a] -> [a]
drop' n xs
  | n <= 0 = xs
drop' _ [] = []
drop' n (_:xs) = drop' (n - 1) xs

-- |
--
-- >>> 2 `exists` [1, 2, 3]
-- True
--
-- > 2 == 1 || 2 `exists` [2, 3]
-- > 2 == 2 || 2 `exists` [3]
exists :: (Eq a) => a -> [a] -> Bool
exists _ [] = False
exists p (x:xs)
  | p == x = True
  | otherwise = p `exists` xs

-- |
-- >>> lookup' "bar" [("foo", "foo@example.net"), ("bar", "bar@example.net")]
-- Just "bar@example.net"
--
-- > if ("foo" == "bar") then (Just "foo@example.net") else (lookup' "bar" [("bar", "bar@example.net")])
-- > if ("bar" == "bar") then (Just "bar@example.net") else (lookup' "bar" [])
--
-- >>> lookup' "baz" [("foo", "foo@example.net"), ("bar", "bar@example.net")]
-- Nothing
--
-- > if ("foo" == "baz") then (Just "foo@example.net") else (lookup' "bar" [("bar", "bar@example.net")])
-- > if ("bar" == "baz") then (Just "bar@example.net") else (lookup' "bar" [])
lookup' :: (Eq k) => k -> [(k, v)] -> Maybe v
lookup' _ [] = Nothing
lookup' key ((k, v):xs)
  | k == key = Just v
  | otherwise = lookup' key xs

-- |
--
-- >>> maximum' [2, 5, 1]
-- 5
--
-- > max 2 (maximum' [5, 1])
-- > max 2 (max 5 (maximum' [1]))
-- > max 2 (max 5 1)
-- > max 2 5
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- |
--
-- >>> quicksort [5, 1, 9, 4, 6, 7 ,3]
-- [1,3,4,5,6,7,9]
--
-- > [1, 4, 3] ++ [5] ++ [9, 6, 7]
-- > ([] ++ [1] ++ [4, 3]) ++ [5] ++ ([6, 7] ++ [9] ++ [])
-- > ([1] ++ ([] ++ [3] ++ [4])) ++ [5] ++ (([] ++ [6] ++ [7]) ++ 9 ++ [])
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
  where smaller = [p | p <- xs, p <= x]
        larger = [p | p <- xs, p > x]

-- |
--
-- >>> take 5 (repeat' "Foo")
-- ["Foo","Foo","Foo","Foo","Foo"]
repeat' :: a -> [a]
repeat' x = x : repeat' x

-- |
--
-- >>> replicate' 3 "Foo"
-- ["Foo","Foo","Foo"]
--
-- > "Foo" : replicate' 2 "Foo"
-- > "Foo" : ("Foo" : replicate' 1 "Foo")
-- > "Foo" : ("Foo" : ("Foo" : replicate' 0 "Foo"))
-- > "Foo" : ("Foo" : ("Foo" : []))
replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

-- |
--
-- >>> reverse' [1, 2, 3, 4]
-- [4,3,2,1]
--
-- > (reverse' [2, 3, 4]) ++ [1]
-- > ((reverse' [3, 4]) ++ [2]) ++ [1]
-- > (((reverse' [4]) ++ [3]) ++ [2]) ++ [1]
-- > ((((reverse' []) ++ [4]) ++ [3]) ++ [2]) ++ [1]
-- > (((([]) ++ [4]) ++ [3]) ++ [2]) ++ [1]
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- |
--
-- >>> take' 3 [1, 2, 3, 4]
-- [1,2,3]
--
-- > 1 : (take' 2 [2, 3, 4])
-- > 1 : (2 : (take' 1 [3, 4]))
-- > 1 : (2 : (3 : (take' 0 [4])))
-- > 1 : (2 : (3 : []))
take' :: Int -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n - 1) xs

-- |
--
-- >>> unique [1, 2, 2, 3]
-- [1,2,3]
--
-- > 1 : unique [p | p <- [2, 2, 3], p /= 1]
-- > 1 : unique [2, 2, 3]
-- > 1 : (2 : unique [p | p <- [2, 3], p /= 2])
-- > 1 : (2 : unique [3])
-- > 1 : (2 : (3 : unique [p | p <- [], p /= 3]))
-- > 1 : (2 : (3 : unique []))
-- > 1 : (2 : (3 : []))
unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs) = x : unique [p | p <- xs, p /= x]

-- |
--
-- >>> zip' [1..4] ["One", "Two", "Three"]
-- [(1,"One"),(2,"Two"),(3,"Three")]
--
-- > (1, "One") : zip' [2, 3, 4] ["Two", "Three"]
-- > (1, "One") : ((2, "Two") : zip' [3, 4] ["Three"])
-- > (1, "One") : ((2, "Two") : ((3, "Three") : zip' [4] []))
-- > (1, "One") : ((2, "Two") : ((3, "Three") : []))
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
