module Main where

main :: IO ()
main = do
  putStrLn "Checking Maybe Monad Law ...."
  checkMonadLaw 1 (Just 2) (const Nothing) (\x -> Just (x + 1))
  putStrLn ""

  putStrLn "Checking List Monad Law ...."
  checkMonadLaw "Foo" ["Bar"] (const []) (\x -> [x ++ "!"])
  putStrLn ""

checkMonadLaw :: (Eq a, Monad m, Eq (m a)) =>
  a -> m a -> (a -> m a) -> (a -> m a) -> IO ()
checkMonadLaw x m f g = do
  putStrLn "1. return x >>= f == f x"
  doReport $ (return x >>= f) == f x && (return x >>= g) == g x

  putStrLn "2. m >>= return == m"
  doReport $ (m >>= return) == m

  putStrLn "3. (m >>= f) >>= g == m >>= (\\x -> f x >>= g)"
  doReport $ ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))

doReport :: Bool -> IO ()
doReport b = putStrLn $ if b then "... OK" else "... NG"
