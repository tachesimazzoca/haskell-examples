module Example.Monoid where

import Control.Applicative
import Data.Monoid

-- |
--
-- >>> CharList "Foo"
-- CharList {getCharList = "Foo"}
-- >>> CharList "Foo" == CharList "Foo"
-- True
-- >>> CharList "Foo" == CharList "Bar"
-- False
-- >>> getCharList $ CharList "Foo"
-- "Foo"
newtype CharList
  = CharList { getCharList :: String }
  deriving (Eq, Show)

-- |
--
-- >>> getPair $ Pair (1, 2)
-- (1,2)
-- >>> getPair $ fmap (+ 1) (Pair (1, 2))
-- (2,2)
newtype Pair b a
  = Pair { getPair :: (a, b) }
  deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair (x, y)) = Pair (f x, y)


-- |
--
-- >>> getZipList' $ ZipList' [1..5]
-- [1,2,3,4,5]
-- >>> getZipList' $ fmap (+ 1) (ZipList' [1..2])
-- [2,3]
-- >>> getZipList' $ ZipList' (repeat (+ 1)) <*> ZipList' [1..2]
-- [2,3]
-- >>> getZipList' $ (+) <$> ZipList' [0..] <*> ZipList' [0..9]
-- [0,2,4,6,8,10,12,14,16,18]
newtype ZipList' a = ZipList' { getZipList' :: [a] }

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' (map f xs)

instance Applicative ZipList' where
  pure x = ZipList' (repeat x)
  ZipList' fs <*> ZipList' xs = ZipList' (zipWith id fs xs)

-- |
--
-- >>> mempty :: Any'
-- Any' {getAny' = False}
-- >>> getAny' $ mempty
-- False
-- >>> getAny' $ Any' True
-- True
-- >>> getAny' $ Any' True `mappend` mempty
-- True
-- >>> getAny' $ Any' True `mappend` Any' True
-- True
-- >>> getAny' $ mconcat . map Any' $ [False, True]
-- True
newtype Any' = Any' { getAny' :: Bool }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid Any' where
  mempty = Any' False
  mappend (Any' a) (Any' b) = Any' (a || b)

-- |
--
-- >>> mempty :: All'
-- All' {getAll' = False}
-- >>> getAll' $ mempty
-- False
-- >>> getAll' $ All' True
-- True
-- >>> getAll' $ All' True `mappend` mempty
-- False
-- >>> getAll' $ All' True `mappend` All' True
-- True
-- >>> getAll' $ mconcat . map All' $ [False, True]
-- False
newtype All' = All' { getAll' :: Bool }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid All' where
  mempty = All' False
  mappend (All' a) (All' b) = All' (a && b)

-- |
--
-- >>> mempty :: (Num a) => (Sum' a)
-- Sum' {getSum' = 0}
-- >>> getSum' $ mempty
-- 0
-- >>> getSum' $ Sum' 2
-- 2
-- >>> getSum' $ Sum' 2 `mappend` mempty
-- 2
-- >>> getSum' $ Sum' 2 `mappend` Sum' 3
-- 5
-- >>> getSum' $ mconcat . map Sum' $ [1..15]
-- 120
newtype Sum' a
  = Sum' { getSum' :: a }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Sum' a) where
  mempty = Sum' 0
  mappend (Sum' a) (Sum' b) = Sum' (a + b)

-- |
--
-- >>> mempty :: (Num a) => (Product' a)
-- Product' {getProduct' = 1}
-- >>> getProduct' $ mempty
-- 1
-- >>> getProduct' $ Product' 2
-- 2
-- >>> getProduct' $ Product' 2 `mappend` mempty
-- 2
-- >>> getProduct' $ Product' 2 `mappend` Product' 3
-- 6
-- >>> getProduct' $ mconcat . map Product' $ [1..5]
-- 120
newtype Product' a
  = Product' { getProduct' :: a }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Product' a) where
  mempty = Product' 1
  mappend (Product' a) (Product' b) = Product' (a * b)

-- | Compare two strings of characters, number of vowels, in alphabetical order.
--
-- >>> LT `mappend` EQ
-- LT
-- >>> LT `mappend` LT
-- LT
-- >>> LT `mappend` GT
-- LT
-- >>> EQ `mappend` EQ
-- EQ
-- >>> EQ `mappend` LT
-- LT
-- >>> EQ `mappend` GT
-- GT
-- >>> GT `mappend` EQ
-- GT
-- >>> GT `mappend` LT
-- GT
-- >>> GT `mappend` GT
-- GT
-- >>> "abcd" `compare` "zyx"
-- LT
-- >>> "abcd" `compareS` "zyx"
-- GT
-- >>> "zyx" `compare` "abc"
-- GT
-- >>> "zyx" `compareS` "abc"
-- LT
-- >>> "abc" `compare` "aba"
-- GT
-- >>> "abc" `compareS` "aba"
-- LT
compareS :: String -> String -> Ordering
compareS x y =
  (length x `compare` length y) `mappend`
  (vowels x `compare` vowels y) `mappend`
  (x `compare` y)
  where vowels = length . filter (`elem` "aeiou")
