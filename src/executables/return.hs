module Main where

main :: IO ()
main = do
  return ()
  putStrLn "In imperative languages, return usually ends the execution of a method or subroutine."
  msg <- return "In Haskell, it makes an I/O action out of a pure value."
  putStrLn msg
