module Example.Monad where

import Control.Monad.Instances ()
import Control.Monad.Writer

-- |
--
-- >>> half 0
-- Nothing
-- >>> half (-1)
-- Nothing
-- >>> half 1
-- Nothing
-- >>> half 2
-- Just 1
half :: Int -> Maybe Int
half 0 = Nothing
half x
     | x < 0     = Nothing
     | odd x     = Nothing
     | otherwise = Just (x `div` 2)

-- |
--
-- >>> halfAndHalf 0
-- Nothing
-- >>> halfAndHalf (-1)
-- Nothing
-- >>> halfAndHalf 1
-- Nothing
-- >>> halfAndHalf 2
-- Nothing
-- >>> halfAndHalf 3
-- Nothing
-- >>> halfAndHalf 4
-- Just 1
-- >>> halfAndHalf 8
-- Just 2
halfAndHalf :: Int -> Maybe Int
halfAndHalf x = Just x >>= half >>= half

-- |
--
-- >>> halfWith 1 2
-- Just 1
-- >>> halfWith 2 8
-- Just 2
halfWith :: Int -> Int -> Maybe Int
halfWith n = foldr (<=<) return (replicate n half)

-- | The bind function like ">>="
--
-- >>> (Just 2) `comb` (\x -> Just (x * 2))
-- Just 4
-- >>> (Just 2) `comb` (const Nothing) `comb` (\x -> Just (x * 2))
-- Nothing
comb :: Maybe a -> (a -> Maybe b) -> Maybe b
comb Nothing _ = Nothing
comb (Just x) f = f x

-- | Compose two monadic functions like "<=<"
--
-- >>> (Just) `comp` (\x -> Just (x * 2)) $ 2
-- Just 4
-- >>> (const Nothing) `comp` (\x -> Just (x * 2)) $ 2
-- Nothing
comp :: (b -> Maybe c) -> (a -> Maybe b) -> a -> Maybe c
comp f g x = g x `comb` f

-- |
--
-- >>> halfAndHalf' 2
-- Nothing
-- >>> halfAndHalf' 4
-- Just 1
-- >>> halfAndHalf' 8
-- Just 2
halfAndHalf' :: Int -> Maybe Int
halfAndHalf' x = Just x `comb` half `comb` half

-- |
--
-- >>> halfWith' 1 2
-- Just 1
-- >>> halfWith' 2 8
-- Just 2
halfWith' :: Int -> Int -> Maybe Int
halfWith' n = foldr comp return (replicate n half)

data BloodType
  = BloodTypeA
  | BloodTypeB
  | BloodTypeAB
  | BloodTypeO
  deriving (Show)

type Donor = (BloodType, [BloodType])

-- |
--
-- >>> return (BloodTypeA, []) >>= donateBlood BloodTypeA >>= donateBlood BloodTypeO
-- Just (BloodTypeA,[BloodTypeO,BloodTypeA])
-- >>> return (BloodTypeA, []) >>= donateBlood BloodTypeA >>= donateBlood BloodTypeB >>= donateBlood BloodTypeO
-- Nothing
-- >>> return (BloodTypeB, []) >>= donateBlood BloodTypeB >>= donateBlood BloodTypeO
-- Just (BloodTypeB,[BloodTypeO,BloodTypeB])
-- >>> return (BloodTypeB, []) >>= donateBlood BloodTypeB >>= donateBlood BloodTypeA >>= donateBlood BloodTypeO
-- Nothing
-- >>> return (BloodTypeO, []) >>= donateBlood BloodTypeO >>= donateBlood BloodTypeO
-- Just (BloodTypeO,[BloodTypeO,BloodTypeO])
-- >>> return(BloodTypeO, []) >>= donateBlood BloodTypeO >>= donateBlood BloodTypeA >>= donateBlood BloodTypeO
-- Nothing
-- >>> return (BloodTypeAB, []) >>= donateBlood BloodTypeB >>= donateBlood BloodTypeA
-- Just (BloodTypeAB,[BloodTypeA,BloodTypeB])
-- >>> return (BloodTypeAB, []) >>= donateBlood BloodTypeO >>= donateBlood BloodTypeB >>= donateBlood BloodTypeA
-- Just (BloodTypeAB,[BloodTypeA,BloodTypeB,BloodTypeO])
donateBlood :: BloodType -> Donor -> Maybe Donor
donateBlood BloodTypeB  (BloodTypeA, _) = Nothing
donateBlood BloodTypeAB (BloodTypeA, _) = Nothing
donateBlood BloodTypeA  (BloodTypeB, _) = Nothing
donateBlood BloodTypeAB (BloodTypeB, _) = Nothing
donateBlood BloodTypeA  (BloodTypeO, _) = Nothing
donateBlood BloodTypeB  (BloodTypeO, _) = Nothing
donateBlood BloodTypeAB (BloodTypeO, _) = Nothing
donateBlood x (y, xs)                   = Just (y, x:xs)

-- |
--
-- >>> testDonateBloodTypeA
-- Just (BloodTypeA,[BloodTypeO,BloodTypeA])
testDonateBloodTypeA :: Maybe Donor
testDonateBloodTypeA = do
  donor <- Just (BloodTypeA, [])
  (x, xs) <- donateBlood BloodTypeA donor
  donateBlood BloodTypeO (x, xs)

-- |
--
-- >>> testDonateBloodTypeO
-- Nothing
testDonateBloodTypeO :: Maybe Donor
testDonateBloodTypeO = do
  donor <- Just (BloodTypeO, [])
  -- it should fail the pattern match, and replace the lines with Nothing.
  (x, xs) <- donateBlood BloodTypeA donor
  donateBlood BloodTypeO (x, xs)

-- |
--
-- >>> return (BloodTypeA, []) >>= donateBlood' BloodTypeA >>= donateBlood' BloodTypeO
-- Right (BloodTypeA,[BloodTypeO,BloodTypeA])
-- >>> return (BloodTypeA, []) >>= donateBlood' BloodTypeA >>= donateBlood' BloodTypeB >>= donateBlood' BloodTypeO
-- Left (BloodTypeA,BloodTypeB)
donateBlood' :: BloodType -> Donor -> Either (BloodType, BloodType) Donor
donateBlood' BloodTypeB  (BloodTypeA, _) = Left (BloodTypeA, BloodTypeB)
donateBlood' BloodTypeAB (BloodTypeA, _) = Left (BloodTypeA, BloodTypeAB)
donateBlood' BloodTypeA  (BloodTypeB, _) = Left (BloodTypeB, BloodTypeA)
donateBlood' BloodTypeAB (BloodTypeB, _) = Left (BloodTypeB, BloodTypeAB)
donateBlood' BloodTypeA  (BloodTypeO, _) = Left (BloodTypeO, BloodTypeA)
donateBlood' BloodTypeB  (BloodTypeO, _) = Left (BloodTypeO, BloodTypeB)
donateBlood' BloodTypeAB (BloodTypeO, _) = Left (BloodTypeO, BloodTypeAB)
donateBlood' x (y, xs)                   = Right (y, x:xs)

-- |
--
-- >>> listOfTuples
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
-- >>> [1..2] >>= \x -> ['a'..'b'] >>= \y -> [(x, y)]
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
-- >>> [ (x, y) | x <- [1..2], y <- ['a'..'b'] ]
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
listOfTuples :: [(Int, Char)]
listOfTuples = do
  k <- [1..2]
  v <- ['a'..'b']
  return (k, v)

-- |
--
-- >>> fizzbuzz [1..5]
-- [(1,"1"),(2,"2"),(3,"fizz"),(4,"4"),(5,"buzz")]
fizzbuzz :: [Int] -> [(Int, String)]
fizzbuzz xs = do
  n <- xs
  let f x
        | fizz && buzz = "fizzbuzz"
        | fizz = "fizz"
        | buzz = "buzz"
        | otherwise = show x
        where fizz = x `mod` 3 == 0
              buzz = x `mod` 5 == 0
  return (n, f n)

-- | Add a point in the range between 0 and 100.
--
-- >>> addPoint 0 0
-- Just 0
-- >>> addPoint 100 0
-- Just 100
-- >>> addPoint (-1) 0
-- Nothing
-- >>> Just 0 >>= addPoint 100 >>= addPoint 10
-- Nothing
-- >>> Just 0 >>= addPoint 100 >>= addPoint (-101) >>= addPoint 10
-- Nothing
-- >>> Just 0 >>= addPoint 100 >>= addPoint (-10)
-- Just 90
-- >>> Just 0 >>= addPoint 100 >> Nothing >>= addPoint (-10)
-- Nothing
-- >>> Just 0 >>= addPoint 100 >> Just 20 >>= addPoint (-10)
-- Just 10
addPoint :: Int -> Int -> Maybe Int
addPoint x y =
  if z <= 100 && z >= 0 then Just z else Nothing
    where z = x + y

-- |
--
-- >>> Just 0 >>= addPoint 100 >>= addPoint (-10)
-- Just 90
-- >>> Just 0 >>= addPoint 100 >>= blockPoint >>= addPoint (-10)
-- Nothing
blockPoint :: Int -> Maybe Int
blockPoint _ = Nothing

-- ** join

-- |
--
-- >>> join' $ Just (Just 1)
-- Just 1
-- >>> runWriter $ join' $ writer (writer (1, ["Foo"]), ["Bar"])
-- (1,["Bar","Foo"])
join' :: (Monad m) => m (m a) -> m a
join' = join
--join' mm = do
--  m <- mm
--  m

-- ** liftM

-- |
--
-- >>> liftM' (+ 1) (Just 1)
-- Just 2
-- >>> fmap (+ 1) (Just 1)
-- Just 2
-- >>> liftM' (+ 1) [1..3]
-- [2,3,4]
-- >>> fmap (+ 1) [1..3]
-- [2,3,4]
-- >>> runWriter $ liftM' (+ 1) $ writer (1, "Foo")
-- (2,"Foo")
-- >>> runWriter $ fmap (+ 1) $ writer (1, "Foo")
-- (2,"Foo")
liftM' :: (Monad m) => (a -> b) -> m a -> m b
liftM' = liftM
--liftM' f m = do
--  x <- m
--  return (f x)

-- ** filterM

-- |
--
-- >>> runWriter $ keepLowerCase "foo"
-- (True,[])
-- >>> runWriter $ keepLowerCase "Bar"
-- (False,["Bar"])
-- >>> runWriter $ filterM keepLowerCase ["Foo", "bar", "123"]
-- (["bar"],["Foo","123"])
keepLowerCase :: String -> Writer [String] Bool
keepLowerCase s =
  let
    xs = [ x | x <- s, x `notElem` ['a'..'z'] ]
  in
    case xs of
      [] -> return True
      _ -> do
        tell [s]
        return False

-- |
--
-- >>> powerset []
-- [[]]
-- >>> powerset ["Foo"]
-- [["Foo"],[]]
-- >>> powerset ["Foo","Bar","Buz"]
-- [["Foo","Bar","Buz"],["Foo","Bar"],["Foo","Buz"],["Foo"],["Bar","Buz"],["Bar"],["Buz"],[]]
-- >>> [ x ++ y ++ z | x <- [["Foo"],[]], y <- [["Bar"],[]], z <- [["Buz"], []] ]
-- [["Foo","Bar","Buz"],["Foo","Bar"],["Foo","Buz"],["Foo"],["Bar","Buz"],["Bar"],["Buz"],[]]
powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

-- ** foldM

-- |
--
-- >>> foldM divNatural 100 [2, 2]
-- Just 25
-- >>> foldM divNatural 100 [2, 3]
-- Just 16
-- >>> foldM divNatural 100 [2, 0]
-- Nothing
-- >>> foldM divNatural 100 [2, -2]
-- Nothing
divNatural :: Int -> Int -> Maybe Int
divNatural acc x
          | x > 0    = Just (acc `div` x)
          | otherwise = Nothing
