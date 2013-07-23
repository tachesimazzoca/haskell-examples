module Example.Writer where

import Control.Monad.Writer

-- |
--
-- >>> runWriter $ logNumber 3 >> logNumber 5
-- (5,["3","5"])
-- >>> runWriter $ logNumber 3 >> tell ["foo"] >> logNumber 5
-- (5,["3","foo","5"])
-- >>> runWriter $ logNumber 3 >> logNumber 5 >> tell ["foo"]
-- ((),["3","5","foo"])
logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, [show x])

-- |
--
-- >>> runWriter $ calcWithLog (+) 3 5
-- (8,["3","5","3,5"])
-- >>> runWriter $ calcWithLog (*) 3 5
-- (15,["3","5","3,5"])
calcWithLog :: (Int -> Int -> Int) -> Int -> Int -> Writer [String] Int
calcWithLog f a b = do
  a' <- logNumber a
  b' <- logNumber b
  tell [ show a' ++ "," ++ show b' ]
  return (f a' b')

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)
  (DiffList f) `mappend` (DiffList g) = DiffList (f . g)

-- |
--
-- >>> fromDiffList $ DiffList ([1..3] ++)
-- [1,2,3]
fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

-- |
--
-- >>> fromDiffList $ toDiffList [1..3] `mappend` toDiffList [4..6]
-- [1,2,3,4,5,6]
toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

-- |
--
-- >>> mapM_ putStrLn . fromDiffList . snd . runWriter $ countDown 5
-- 0
-- 1
-- 2
-- 3
-- 4
-- 5
countDown :: Int -> Writer (DiffList String) ()
countDown 0 = tell (toDiffList ["0"])
countDown x = do
  countDown (x - 1)
  tell (toDiffList [show x])
