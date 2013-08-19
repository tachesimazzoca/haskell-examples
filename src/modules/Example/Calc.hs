module Example.Calc where

import Control.Monad.Error

data CalcError
  = Err {
    location :: Int,
    reason:: String
  } deriving Show

instance Error CalcError where
  noMsg  = Err 0 "CalcError"
  strMsg = Err 0

type Calc = Either CalcError

-- |
--
-- >>> parseNum "123" 0
-- Right 123
-- >>> parseNum "abc" 1
-- Left (Err {location = 1, reason = "Invalid string 'abc'"})
parseNum :: (Num a) => String -> Int -> Calc a
parseNum s n =
  case (reads s :: [(Int, String)]) of
    [(a, "")] -> return $ fromIntegral a
    _         -> throwError $ Err n ("Invalid string '" ++ s ++ "'")

-- |
--
-- >>> parse "1 2 3"
-- Right [1,2,3]
-- >>> parse "1 a 2"
-- Left (Err {location = 2, reason = "Invalid string 'a'"})
parse :: (Num a) => String -> Calc [a]
parse s = parse' (words s) [] 1
  where parse' [] vs _ = return vs
        parse' (x:xs) vs n = do
          v <- parseNum x n
          parse' xs (vs ++ [v]) (n + 1)

-- |
--
-- >>> calc (sum) "1 2 3"
-- "6"
-- >>> calc (sum) "1 2 a"
-- "Parse error at item 3: Invalid string 'a'"
calc :: (Show a, Num a) => ([a] -> a) -> String -> String
calc f a =
  let (Right s) = do {
    vs <- parse a;
    return $ show (f vs);
  } `catchError` printError
  in s
  where
    printError e = return $
      "Parse error at item " ++ show (location e) ++ ": " ++ reason e
