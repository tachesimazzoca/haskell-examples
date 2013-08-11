module Main where

import Control.Monad

main :: IO ()
main = do
  putStrLn "Checking Maybe MonadPlus Law ...."
  checkMonadPlusLaw (mzero :: Maybe String) (Just "Foo") (\x -> Just (x ++ "!"))
  putStrLn ""

  putStrLn "Checking List MonadPlus Law ...."
  checkMonadPlusLaw (mzero :: [String]) ["Foo"] (\x -> [x ++ "!"])
  putStrLn ""

checkMonadPlusLaw :: (MonadPlus m, Eq (m a)) =>
  m a -> m a -> (a -> m a) -> IO ()
checkMonadPlusLaw z m f = do
  putStrLn "1. mzero >>= f == mzero"
  doReport $ (mzero >>= f) == mzero

  putStrLn "2. m >>= (\\x -> mzero) == mzero"
  doReport $ (m >>= (const mzero)) == z

  putStrLn "3. mzero `mplus` m == m"
  doReport $ (mzero `mplus` m) == m

  putStrLn "4. m `mplus` mzero == m"
  doReport $ (m `mplus` mzero) == m

doReport :: Bool -> IO ()
doReport b = putStrLn $ if b then "... OK" else "... NG"
