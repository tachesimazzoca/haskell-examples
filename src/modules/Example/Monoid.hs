module Example.Monoid where

import Control.Applicative (
    Applicative
  , pure
  , (<*>)
  )

-- $setup
-- >>> import Control.Applicative ((<$>))

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
