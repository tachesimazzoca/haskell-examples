module Example.Sheep where

import Control.Monad

-- $setup
-- >>> let foo = (Sheep "foo" Nothing Nothing)
-- >>> let bar = (Sheep "bar" Nothing Nothing)
-- >>> let baz = (Sheep "baz" (Just bar) (Just foo))
-- >>> let qux = (Sheep "qux" (Just baz) Nothing)
-- >>> let qqux = (Sheep "qqux" Nothing (Just qux))

-- |
--
-- >>> foo
-- Sheep {name = "foo", mother = Nothing, father = Nothing}
-- >>> return foo >>= father >>= father
-- Nothing
-- >>> return qqux >>= father >>= mother >>= mother
-- Just (Sheep {name = "bar", mother = Nothing, father = Nothing})
data Sheep
  = Sheep {
    name :: String,
    mother :: Maybe Sheep,
    father :: Maybe Sheep
  } deriving Show

-- |
--
-- >>> sheepName (Just foo)
-- Just "foo"
-- >>> sheepName [foo, bar]
-- ["foo","bar"]
sheepName :: (Monad m) => m Sheep -> m String
--sheepName m = m >>= \x -> return (name x) >>= return
sheepName m = do
  x <- m
  return (name x)

-- |
--
-- >>> ((maybeToMonad (Just 1)) :: Maybe Int) == ((return 1) :: Maybe Int)
-- True
-- >>> ((maybeToMonad Nothing) :: Maybe Int) == (mzero :: Maybe Int)
-- True
maybeToMonad :: (MonadPlus m) => Maybe a -> m a
maybeToMonad Nothing = mzero
maybeToMonad (Just a) = return a

-- |
--
-- >>> (parent foo) :: Maybe Sheep
-- Nothing
-- >>> (parent baz) :: Maybe Sheep
-- Just (Sheep {name = "bar", mother = Nothing, father = Nothing})
-- >>> sheepName $ ((parent qux) :: Maybe Sheep)
-- Just "baz"
-- >>> sheepName $ ((parent qqux) :: Maybe Sheep)
-- Just "qux"
-- >>> (parent foo) :: [Sheep]
-- []
-- >>> sheepName $ ((parent baz) :: [Sheep])
-- ["bar","foo"]
-- >>> sheepName $ ((parent qux) :: [Sheep])
-- ["baz"]
-- >>> sheepName $ ((parent qqux) :: [Sheep])
-- ["qux"]
parent :: (MonadPlus m) => Sheep -> m Sheep
parent s = maybeToMonad (mother s) `mplus`
           maybeToMonad (father s)

-- |
--
-- >>> (grandParent baz) :: Maybe Sheep
-- Nothing
-- >>> (grandParent qux) :: Maybe Sheep
-- Just (Sheep {name = "bar", mother = Nothing, father = Nothing})
-- >>> sheepName $ ((grandParent qqux) :: Maybe Sheep)
-- Just "baz"
-- >>> sheepName $ ((grandParent baz) :: [Sheep])
-- []
-- >>> sheepName $ ((grandParent qux) :: [Sheep])
-- ["bar","foo"]
-- >>> sheepName $ ((grandParent qqux) :: [Sheep])
-- ["baz"]
grandParent :: (MonadPlus m) => Sheep -> m Sheep
grandParent s = (maybeToMonad (mother s) >>= parent) `mplus`
                (maybeToMonad (father s) >>= parent)
