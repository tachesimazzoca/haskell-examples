module Main where

import Control.Monad

main :: IO ()
main = do
  putStr "Enter something (empty to exit) > "
  str <- getLine
  unless (null str) main

  putStrLn "The stacked line will be printed after the recursive call."
  putStrLn str
