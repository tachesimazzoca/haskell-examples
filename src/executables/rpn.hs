module Main (main) where

import System (getArgs)

main :: IO ()
main = do
  args <- getArgs
  print $ calc args

-- | Calc in Reverse Polish Notation.
--
-- >>> calc []
-- 0
-- >>> calc ["1", "2", "+"]
-- 3
-- >>> calc ["3", "5", "1", "2", "+", "-", "*"]
-- 6
-- >>> calc ["NaN", "12", "-"]
-- -12
calc :: [String] -> Int
calc xs = head $ (foldl f [] xs) ++ [0]
  where
    f (x:y:xs) "+" = (y + x) : xs
    f (x:y:xs) "-" = (y - x) : xs
    f (x:y:xs) "*" = (y * x) : xs
    f xs x = (fst $ head $ (reads x :: [(Int, String)]) ++ [(0, "")]) : xs
