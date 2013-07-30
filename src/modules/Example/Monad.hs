module Example.Monad where

import Control.Monad.Instances ()

data BloodType
  = BloodTypeA
  | BloodTypeB
  | BloodTypeAB
  | BloodTypeO
  deriving (Show)

type Donor = (BloodType, [BloodType])

-- |
--
-- >>> Just (BloodTypeA, []) >>= donateBlood BloodTypeA >>= donateBlood BloodTypeO
-- Just (BloodTypeA,[BloodTypeO,BloodTypeA])
-- >>> Just (BloodTypeA, []) >>= donateBlood BloodTypeA >>= donateBlood BloodTypeB >>= donateBlood BloodTypeO
-- Nothing
-- >>> Just (BloodTypeB, []) >>= donateBlood BloodTypeB >>= donateBlood BloodTypeO
-- Just (BloodTypeB,[BloodTypeO,BloodTypeB])
-- >>> Just (BloodTypeB, []) >>= donateBlood BloodTypeB >>= donateBlood BloodTypeA >>= donateBlood BloodTypeO
-- Nothing
-- >>> Just (BloodTypeO, []) >>= donateBlood BloodTypeO >>= donateBlood BloodTypeO
-- Just (BloodTypeO,[BloodTypeO,BloodTypeO])
-- >>> Just (BloodTypeO, []) >>= donateBlood BloodTypeO >>= donateBlood BloodTypeA >>= donateBlood BloodTypeO
-- Nothing
-- >>> Just (BloodTypeAB, []) >>= donateBlood BloodTypeB >>= donateBlood BloodTypeA
-- Just (BloodTypeAB,[BloodTypeA,BloodTypeB])
-- >>> Just (BloodTypeAB, []) >>= donateBlood BloodTypeO >>= donateBlood BloodTypeB >>= donateBlood BloodTypeA
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
-- >>> Right (BloodTypeA, []) >>= donateBlood' BloodTypeA >>= donateBlood' BloodTypeO
-- Right (BloodTypeA,[BloodTypeO,BloodTypeA])
-- >>> Right (BloodTypeA, []) >>= donateBlood' BloodTypeA >>= donateBlood' BloodTypeB >>= donateBlood' BloodTypeO
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
