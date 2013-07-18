module Example.Tree where

import qualified Data.Foldable as F
import Data.Monoid

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving (Show)

-- |
--
-- >>> let tree = (Node 3 (Node 2 Empty (Node 5 Empty Empty)) (Node 4 Empty Empty))
-- >>> Data.Foldable.foldl (+) 0 tree
-- 14
-- >>> getSum $ Data.Foldable.foldMap Sum tree
-- 14
-- >>> Data.Foldable.foldl (*) 1 tree
-- 120
-- >>> getProduct $ Data.Foldable.foldMap Product tree
-- 120
-- >>> Data.Foldable.foldMap (\x -> [x]) tree
-- [2,5,3,4]
instance F.Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Node x l r) =
    F.foldMap f l `mappend`
    f x           `mappend`
    F.foldMap f r

-- |
--
-- >>> treeInsert 0 Empty
-- Node 0 Empty Empty
-- >>> treeInsert 0 (Node 0 Empty Empty)
-- Node 0 Empty Empty
-- >>> treeInsert 1 (Node 0 Empty Empty)
-- Node 0 Empty (Node 1 Empty Empty)
-- >>> foldr treeInsert Empty [1, 3, 2]
-- Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x Empty = Node x Empty Empty
treeInsert x (Node y l r)
  | x < y     = Node y (treeInsert x l) r
  | x > y     = Node y l (treeInsert x r)
  | otherwise = Node x l r

-- |
--
-- >>> treeElem 1 Empty
-- False
-- >>> treeElem 3 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty))
-- True
-- >>> treeElem 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty))
-- False
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ Empty = False
treeElem x (Node y l r)
  | x < y     = treeElem x l
  | x > y     = treeElem x r
  | otherwise = True
