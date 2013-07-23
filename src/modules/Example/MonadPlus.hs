module Example.MonadPlus where

import Control.Monad

-- |
--
-- >>> jpegOnly []
-- []
-- >>> let imgs = ["foo.gif", "bar.jpg", "fuga.png", "baz.jpg"]
-- >>> jpegOnly imgs
-- ["bar.jpg","baz.jpg"]
-- >>> imgs >>= \x -> guard (dropWhile (/= '.') x == ".jpg") >> return x
-- ["bar.jpg","baz.jpg"]
-- >>> [ x | x <- imgs, dropWhile (/= '.') x == ".jpg" ]
-- ["bar.jpg","baz.jpg"]
jpegOnly :: [String] -> [String]
jpegOnly xs = do
  x <- xs
  guard (dropWhile (/= '.') x == ".jpg")
  return x

-- |
--
-- >>> moveKnight (6, 2)
-- [(5,4),(7,4),(4,1),(4,3),(8,1),(8,3)]
-- >>> (6, 1) `elem` (return (6, 2) >>= moveKnight >>= moveKnight >>= moveKnight)
-- True
-- >>> (7, 3) `elem` (return (6, 2) >>= moveKnight >>= moveKnight >>= moveKnight)
-- False
moveKnight :: (Int, Int) -> [(Int, Int)]
moveKnight (x, y) = do
  (x', y') <- [
      (x - 1, y - 2)
    , (x - 1, y + 2)
    , (x + 1, y - 2)
    , (x + 1, y + 2)
    , (x - 2, y - 1)
    , (x - 2, y + 1)
    , (x + 2, y - 1)
    , (x + 2, y + 1)
    ]
  guard (x' `elem` [1..8] && y' `elem` [1..8])
  return (x', y')

-- |
--
-- >>> (6, 1) `elem` inMany 3 (6,2)
-- True
-- >>> (7, 3) `elem` inMany 3 (6,2)
-- False
inMany :: Int -> (Int, Int) -> [(Int, Int)]
inMany x = foldr (<=<) return (replicate x moveKnight)
