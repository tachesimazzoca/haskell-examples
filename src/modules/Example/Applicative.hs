module Example.Applicative where

import Control.Applicative (
    Applicative
  , pure
  , (<*>)
  , (<$>)
  )

-- $setup
-- >>> import Control.Applicative (liftA2)
-- >>> import Data.List (nub)

-- |
--
-- >>> fmap' (+ 2) (Just 3)
-- Just 5
-- >>> Just (+ 2) <*> (Just 3)
-- Just 5
-- >>> fmap (+ 2) (Just 3)
-- Just 5
fmap' :: (Applicative f) => (a -> b) -> f a -> f b
fmap' fn x = pure fn <*> x

-- |
--
-- >>> liftA2' (++) (Just "Foo") (Just "Bar")
-- Just "FooBar"
-- >>> (++) <$> Just "Foo" <*> Just "Bar"
-- Just "FooBar"
-- >>> fmap (++) (Just "Foo") <*> (Just "Bar")
-- Just "FooBar"
-- >>> liftA2' (\a b -> a ++ " vs " ++ b) ["Foo", "Fuga"] ["Bar", "Baz"]
-- ["Foo vs Bar","Foo vs Baz","Fuga vs Bar","Fuga vs Baz"]
liftA2' :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2' fn x y = fn <$> x <*> y

-- |
--
-- >>> sequenceA [Just 1, Just 2, Just 3]
-- Just [1,2,3]
-- >>> foldr (liftA2 (:)) (Just []) [Just 1, Just 2, Just 3]
-- Just [1,2,3]
-- >>> sequenceA [[1, 2], [3, 4]]
-- [[1,3],[1,4],[2,3],[2,4]]
-- >>> [ [x, y] | x <- [1, 2], y <- [3, 4] ]
-- [[1,3],[1,4],[2,3],[2,4]]
-- >>> sequenceA [(> 0), (< 10), even] 10
-- [True,False,True]
-- >>> [ f 10 | f <- [(> 0), (< 10), even] ]
-- [True,False,True]
-- >>> let required = not . null
-- >>> let noBlank = (\x -> nub x /= " ")
-- >>> let maxLength = (\x -> (length x) <= 10)
-- >>> let validate' xs = and $ sequenceA [required, noBlank, maxLength] xs
-- >>> validate' "Foo Bar"
-- True
-- >>> validate' "   "
-- False
-- >>> validate' "Over 10 characters"
-- False
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (\x acc -> (:) <$> x <*> acc) (pure [])
