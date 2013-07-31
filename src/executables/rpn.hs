module Main (main) where

import Control.Monad
import Control.Monad.Writer
import System (getArgs)

main :: IO ()
main = do
  args <- getArgs
  print $ runWriter $ foldM solveRPN (Just []) args

-- | Calc in Reverse Polish Notation.
--
-- >>> runWriter $ solveRPN (Just []) ""
-- (Nothing,[])
-- >>> runWriter $ solveRPN (Just [1, 2]) "+"
-- (Just [3],["2 + 1"])
-- >>> runWriter $ foldM solveRPN (Just []) ["3", "5", "1", "2", "+", "-", "*"]
-- (Just [6],["1 + 2","5 - 3","3 * 2"])
-- >>> runWriter $ foldM solveRPN (Just []) ["12", "Nan", "-"]
-- (Nothing,["Parse error: Nan"])
solveRPN :: Maybe [Int] -> String -> Writer [String] (Maybe [Int])
solveRPN Nothing _ = return Nothing
solveRPN _ ""      = return Nothing
solveRPN (Just (x:y:ys)) "+" = do
  tell [show y ++ " + " ++ show x]
  return (Just ((y + x):ys))
solveRPN (Just (x:y:ys)) "-" = do
  tell [show y ++ " - " ++ show x]
  return (Just ((y - x):ys))
solveRPN (Just (x:y:ys)) "*" = do
  tell [show y ++ " * " ++ show x]
  return (Just ((y * x):ys))
solveRPN (Just ys) y = do
  let m = case (reads y :: [(Int, String)]) of
            [(a, "")] -> Just a
            _         -> Nothing
  case m of
    Just a  -> return ()
    Nothing -> tell ["Parse error: " ++ y]

  return (liftM (:ys) m)
