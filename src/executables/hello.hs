module Main where

main :: IO ()
main = do
  putStr "Your Name >"
  name <- getLine
  putStrLn ("Hello! " ++ name)
