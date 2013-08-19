module Example.Parser where

import Control.Monad
import Data.Char

data Parsed
  = Digit Integer
  | Hex Integer
  | Word String
  deriving Show

-- |
--
-- >>> parseHexDigit (Hex 0) 'x'
-- []
-- >>> parseHexDigit (Hex 0) '1'
-- [Hex 1]
-- >>> parseHexDigit (Hex 0) 'f'
-- [Hex 15]
-- >>> parseHexDigit (Hex 15) 'f'
-- [Hex 255]
parseHexDigit :: Parsed -> Char -> [Parsed]
parseHexDigit (Hex n) c =
  if isHexDigit c then
    return (Hex ((n * 16) + (toInteger (digitToInt c))))
  else
    mzero
parseHexDigit _  _ = mzero

-- |
--
-- >>> parseDigit (Digit 0) 'x'
-- []
-- >>> parseDigit (Digit 0) '2'
-- [Digit 2]
-- >>> parseDigit (Digit 3) '5'
-- [Digit 35]
parseDigit :: Parsed -> Char -> [Parsed]
parseDigit (Digit n) c =
  if isDigit c then
    return (Digit ((n * 10) + (toInteger (digitToInt c))))
  else
    mzero
parseDigit _  _ = mzero

-- |
--
-- >>> parseWord (Word "") 'x'
-- [Word "x"]
-- >>> parseWord (Word "xy") 'z'
-- [Word "xyz"]
parseWord :: Parsed -> Char -> [Parsed]
parseWord (Word s) c =
  if isAlpha c then
    return (Word (s ++ [c]))
  else
    mzero
parseWord _ _ = mzero

-- |
--
-- >>> parseChar (Hex 0) '1'
-- [Hex 1]
-- >>> let acc = ((return (Hex 0)) `mplus` (return (Digit 0)) `mplus` (return (Word ""))) :: [Parsed]
-- >>> acc 
-- [Hex 0,Digit 0,Word ""]
-- >>> acc >>= \x -> parseChar x '1'
-- [Hex 1,Digit 1]
-- >>> acc >>= \x -> parseChar x 'c'
-- [Hex 12,Word "c"]
parseChar :: Parsed -> Char -> [Parsed]
parseChar p c = (parseHexDigit p c) `mplus` (parseDigit p c) `mplus` (parseWord p c)

-- |
--
-- >>> parse "1"
-- [Hex 1,Digit 1]
-- >>> parse "a"
-- [Hex 10,Word "a"]
-- >>> parse "ff"
-- [Hex 255,Word "ff"]
-- >>> parse "fx"
-- [Word "fx"]
parse :: String -> [Parsed]
parse s = do
  acc <-
    (return (Hex 0)  ) `mplus`
    (return (Digit 0)) `mplus`
    (return (Word ""))
  foldM parseChar acc s
