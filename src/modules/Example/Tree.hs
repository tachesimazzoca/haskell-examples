module Example.Tree (
  Tree(..)
, treeInsert
, treeElem
) where

data Tree a
  = Empty
  | Node a (Tree a) (Tree a)
  deriving (Show)

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
