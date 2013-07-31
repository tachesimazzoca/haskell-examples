module Example.Dice where

import Control.Arrow (first, second)
import Data.List (nub)
import Data.Ratio

newtype Prob a = Prob { getProb :: [(a, Rational)] }

-- |
--
-- >>> getProb $ fmap not (Prob [(True, 1 % 2), (False, 1 % 2)])
-- [(False,1 % 2),(True,1 % 2)]
-- >>> getProb $ fmap (+ 1) (Prob [(1, 1 % 2), (2, 1 % 2)])
-- [(2,1 % 2),(3,1 % 2)]
instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (first f) xs

-- |
--
-- >>> getProb $ return True
-- [(True,1 % 1)]
-- >>> getProb $ (Prob [(True, 1 % 2), (False, 1 % 2)]) >> (Prob [(True, 1 % 10), (False, 9 % 10)])
-- [(True,1 % 20),(False,9 % 20),(True,1 % 20),(False,9 % 20)]
instance Monad Prob where
  return x = Prob [(x, 1 % 1)]
  m >>= f  = joinProb (fmap f m)
  fail _   = Prob []

-- |
--
-- >>> getProb $ joinProb (Prob [])
-- []
-- >>> getProb $ joinProb (Prob [(Prob [("Foo", 1 % 2), ("Bar", 1 % 2)], 1 % 4), (Prob [("Fuga", 1 % 1)], 3 % 4)])
-- [("Foo",1 % 8),("Bar",1 % 8),("Fuga",3 % 4)]
joinProb :: Prob (Prob a) -> Prob a
joinProb (Prob xs) = Prob $ concatMap multAll xs
  where multAll (Prob ys, r) = map (second (r *)) ys

-- | Create a die with the number of facets
--
-- >>> getProb $ baseDie 4
-- [(1,1 % 4),(2,1 % 4),(3,1 % 4),(4,1 % 4)]
baseDie :: Int -> Prob Int
baseDie n = Prob $ map (\x -> (x, 1 % fromIntegral n)) [1..n]

-- | Create a cube die
--
-- >>> getProb $ cubeDie
-- [(1,1 % 6),(2,1 % 6),(3,1 % 6),(4,1 % 6),(5,1 % 6),(6,1 % 6)]
cubeDie :: Prob Int
cubeDie = baseDie 6

-- |
--
-- >>> probTrue (Prob [(True, 1 % 3), (False, 1 % 3), (True, 1 % 3)])
-- 2 % 3
probTrue :: Prob Bool -> Rational
probTrue p = foldl (\acc (a, r) -> if a then acc + r else acc) 0 (getProb p)

-- | A probability in the case of rolling two dice and adding the result together.
--
-- See <http://en.wikipedia.org/wiki/Dice#Probability>
--
-- >>> map (\x -> probTrue $ sumFacets cubeDie cubeDie x) [2..12]
-- [1 % 36,1 % 18,1 % 12,1 % 9,5 % 36,1 % 6,5 % 36,1 % 9,1 % 12,1 % 18,1 % 36]
sumFacets :: (Num a) => Prob a -> Prob a -> a -> Prob Bool
sumFacets d1 d2 n = do
  n1 <- d1
  n2 <- d2
  return ((n1 + n2) == n)

-- | A probability in the case of rolling three dice and being the same result.
--
-- >>> let coin = baseDie 2
-- >>> getProb $ sameThreeFacets coin
-- [(True,1 % 8),(False,1 % 8),(False,1 % 8),(False,1 % 8),(False,1 % 8),(False,1 % 8),(False,1 % 8),(True,1 % 8)]
-- >>> probTrue $ sameThreeFacets coin
-- 1 % 4
-- >>> probTrue $ sameThreeFacets cubeDie
-- 1 % 36
-- >>> probTrue $ sameThreeFacets (baseDie 12)
-- 1 % 144
sameThreeFacets :: (Eq a) => Prob a -> Prob Bool
sameThreeFacets d = do
  n1 <- d
  n2 <- d
  n3 <- d
  return (nub [n1, n2, n3] == [n1])
