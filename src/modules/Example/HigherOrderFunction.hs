module Example.HigherOrderFunction where

import Data.Char

-- ** Caesar Chiper
-- |
-- >>> encrypt 2 "abc"
-- "cde"
-- >>> encrypt 16 "Haskell"
-- "Xq\131{u||"
encrypt :: Int -> String -> String
encrypt n = map (chr . (+ n) . ord)

-- |
-- >>> decrypt 2 "cde"
-- "abc"
-- >>> decrypt 16 "Xq\131{u||"
-- "Haskell"
decrypt :: Int -> String -> String
decrypt n = encrypt (negate n)

-- ** Function Composition

rate :: Double -> Int -> Int
rate x y = floor ((fromIntegral y :: Double) * x)

-- |
--
-- >>> discount 10000
-- 9000
discount :: Int -> Int
discount = rate 0.90

-- |
--
-- >>> addTax 9000
-- 9450
addTax :: Int -> Int
addTax = rate 1.05

-- |
--
-- >>> useCoupon 9450
-- 8450
useCoupon :: Int -> Int
useCoupon x = max 0 (x - 1000)

-- |
--
-- >>> addShippingCharge 8450
-- 8765
addShippingCharge :: Int -> Int
addShippingCharge = (+ addTax 300)

-- |
--
-- >>> payment 10000 [discount]
-- 9000
-- >>> payment 10000 [discount, addTax]
-- 9450
-- >>> payment 10000 [discount, addTax, useCoupon]
-- 8450
-- >>> payment 10000 [discount, addTax, useCoupon, addShippingCharge]
-- 8765
-- >>> payment 10000 [rate 0.9, rate 1.05, subtract 1000, (+ rate 1.05 300)]
-- 8765
-- >>> (foldl (.) (+ 0) (reverse [discount, addTax, useCoupon, addShippingCharge])) 10000
-- 8765
-- >>> (foldl (flip (.)) (+ 0) [discount, addTax, useCoupon, addShippingCharge]) 10000
-- 8765
-- >>> (addShippingCharge . useCoupon . addTax . discount) 10000
-- 8765
-- >>> addShippingCharge (useCoupon (addTax (discount 10000)))
-- 8765
payment :: Int -> [Int -> Int] -> Int
payment p xs = foldl (flip (.)) (+ 0) xs p

-- ** Filter

-- |
--
-- >>> filter' even [1..10]
-- [2,4,6,8,10]
-- >>> let notBlank x = not (x `elem` [' ']) in filter' notBlank "DEAD BEEF"
-- "DEADBEEF"
--
-- > filter` even [2..10]
-- > 2 : (filter` even [3..10])
-- > 2 : (filter` even [4..10])
-- > 2 : (4 : (filter` even [5..10]))
-- > ....
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

-- ** FizzBuzz

-- |
--
-- >>> fizzbuzz [1..15]
-- ["1","2","fizz","4","buzz","fizz","7","8","fizz","buzz","11","fizz","13","14","fizzbuzz"]
fizzbuzz :: [Int] -> [String]
fizzbuzz =
  let f x
        | fizz && buzz = "fizzbuzz"
        | fizz = "fizz"
        | buzz = "buzz"
        | otherwise = show x
        where fizz = x `mod` 3 == 0
              buzz = x `mod` 5 == 0
  in map f

-- ** Fold

-- |
--
-- >>> join "," ["Foo", "Bar", "Baz"]
-- "Foo,Bar,Baz"
join :: String -> [String] -> String
join sep = foldl f ""
  where f "" x = x
        f acc x = acc ++ sep ++ x

-- |
--
-- >>> lookupr' "bar" [("foo", "foo-1"), ("bar", "bar-1"), ("foo", "foo-2"), ("bar", "bar-2")]
-- Just "bar-1"
--
-- > if "bar" == "bar" then Just "bar-2" else Nothing
-- > if "foo" == "bar" then Just "foo-2" else Just "bar-2"
-- > if "bar" == "bar" then Just "bar-1" else Just "bar-2"
-- > if "foo" == "bar" then Just "foo-1" else Just "bar-1"
lookupr' :: (Eq k) => k -> [(k, v)] -> Maybe v
lookupr' key = foldr (\(k, v) acc -> if k == key then Just v else acc) Nothing

-- |
--
-- >>> lookupl' "bar" [("foo", "foo-1"), ("bar", "bar-1"), ("foo", "foo-2"), ("bar", "bar-2")]
-- Just "bar-2"
--
-- > if "foo" == "bar" then Just "foo-1" else Nothing
-- > if "bar" == "bar" then Just "bar-1" else Nothing
-- > if "bar" == "foo" then Just "foo-2" else Just "bar-1"
-- > if "bar" == "bar" then Just "bar-2" else Just "bar-1"
lookupl' :: (Eq k) => k -> [(k, v)] -> Maybe v
lookupl' key = foldl (\acc (k, v) -> if k == key then Just v else acc) Nothing

-- ** Lambda

-- |
--
-- >>> surroundWith "<hs:" " />" ["foo", "bar"]
-- ["<hs:foo />","<hs:bar />"]
-- >>> surroundWith "[" "]" (map show [0..9])
-- ["[0]","[1]","[2]","[3]","[4]","[5]","[6]","[7]","[8]","[9]"]
surroundWith :: String -> String -> [String] -> [String]
surroundWith l r = map (\x -> l ++ x ++ r)

-- ** Map

-- |
--
-- >>> map' (+ 1) [1..3]
-- [2,3,4]
--
-- > 1 + 1 : map' (+ 1) [2..3]
-- > 1 + 1 : (2 + 1 : map' (+ 1) [3])
-- > 1 + 1 : (2 + 1 : (3 + 1 : map' (+ 1) []))
-- > 1 + 1 : (2 + 1 : (3 + 1 : []))
--
-- >>> map' (max 0) [-1..1]
-- [0,0,1]
--
-- > (max 0 -1) : map' (max 0) [0..1]
-- > (max 0 -1) : ((max 0 0) : map' (max 0) [1])
-- > (max 0 -1) : ((max 0 0) : ((max 1 0) : map' (max 0) []))
-- > (max 0 -1) : ((max 0 0) : ((max 1 0) : []))
--
-- >>> map' (take 1) ["Foo", "Bar"]
-- ["F","B"]
--
-- > (take 1 "Foo") : map' (take 1) "Bar"
-- > (take 1 "Foo") : ((take 1 "Bar"), map' (take 1) [])
-- > (take 1 "Foo") : ((take 1 "Bar"), [])
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- |
--
-- >>> (listOfNumbering !! 1) "Foo"
-- "1. Foo"
-- >>> (listOfNumbering !! 2) "Bar"
-- "2. Bar"
listOfNumbering :: [String -> String]
listOfNumbering = map (++) ["", "1. ", "2. ", "3. "]

-- |
--
-- >>> (listOfDescribing !! 1) "Foo"
-- "Foo is the 1st"
-- >>> (listOfDescribing !! 2) "Bar"
-- "Bar is the 2nd"
listOfDescribing :: [String -> String]
listOfDescribing = map (flip (++)) ["", " is the 1st", " is the 2nd", " is the 3rd"]

-- ** Scan

-- |
--
-- >>> listOfFiles ["var", "log", "httpd", "access_log"]
-- ["/var","/var/log","/var/log/httpd","/var/log/httpd/access_log"]
listOfFiles :: [String] -> [String]
listOfFiles s = tail $ scanl (\acc x -> acc ++ "/" ++ x) [] s
