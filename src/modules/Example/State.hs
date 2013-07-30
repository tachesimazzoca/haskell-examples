{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Example.State where

import Control.Monad.State
import System.Random

-- |
--
-- >>> runState pop [1..3]
-- (Just 1,[2,3])
-- >>> runState pop []
-- (Nothing,[])
pop :: State [Int] (Maybe Int)
pop = state $ \s -> case s of
    (x:xs) -> (Just x, xs)
    []     -> (Nothing, [])

-- |
--
-- >>> runState pop' [1..3]
-- (Just 1,[2,3])
-- >>> runState pop' []
-- (Nothing,[])
pop' :: State [Int] (Maybe Int)
pop' = do
  s <- get
  case s of
    (x:xs) -> do
      put xs
      return (Just x)
    []     -> do
      put []
      return Nothing

-- |
--
-- >>> runState (push 4) [1..3]
-- ((),[4,1,2,3])
push :: Int -> State [Int] ()
push a = state $ \(xs) -> ((), a:xs)

-- |
--
-- >>> runState (push' 4) [1..3]
-- ((),[4,1,2,3])
push' :: Int -> State [Int] ()
push' a = do
  xs <- get
  put (a:xs)
  return ()

-- |
--
-- >>> runState stackManip [1..3]
-- ((),[4,2,3])
stackManip :: State [Int] ()
stackManip = do
  pop
  push 4
  push 5
  pop
  return ()

-- |
--
-- >>> runState stackEvenFirst [1..3]
-- ((),[2,2,3])
-- >>> runState stackEvenFirst [0..3]
-- ((),[0,1,2,3])
stackEvenFirst :: State [Int] ()
stackEvenFirst = do
  a <- pop
  case a of
    Just x  -> push $ if even x then x else x + 1
    Nothing -> push 0
  return ()

-- |
--
-- >>> fst $ runState (randomPoint 1 10) (mkStdGen 1)
-- (7,10)
-- >>> fst $ runState (randomPoint (-5) 5) (mkStdGen 1)
-- (-3,3)
randomPoint :: Int -> Int -> State StdGen (Int, Int)
randomPoint a b = do
  let m = state random :: (State StdGen Int)
  x <- m
  y <- m
  let f n = n `mod` (abs a + abs b) + a
  return (f x, f y)

-- |
--
-- >>> randomPoint' 1 10 (mkStdGen 1)
-- (7,10)
-- >>> randomPoint' (-5) 5 (mkStdGen 1)
-- (-3,3)
randomPoint' :: Int -> Int -> StdGen -> (Int, Int)
randomPoint' a b g1 =
  let
    (x, g2) = random g1 :: (Int, StdGen)
    (y, _)  = random g2 :: (Int, StdGen)
    f n = n `mod` (abs a + abs b) + a
  in
    (f x, f y)
