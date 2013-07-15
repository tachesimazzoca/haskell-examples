module Main where

import Data.Char
import Data.Maybe

main :: IO ()
main = do
  putStr "Yes/No? (y/n) > "
  yn <- getChar
  putStrLn ""
  let b = yesno yn
  if isNothing b then
    main
  else
    print b

yesno :: Char -> Maybe Bool
yesno x
 | toUpper x == 'Y' = Just True
 | toUpper x == 'N' = Just False
 | otherwise        = Nothing
