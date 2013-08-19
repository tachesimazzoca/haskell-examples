module Example.Kinship where

import Control.Monad
import qualified Data.Map as Map

data Life
  = Life {
    name :: String,
    mother :: Maybe String,
    father :: Maybe String
  } deriving (Eq, Show)

-- $setup
-- >>> let lifeDB = Map.fromList $ map (\x -> (name x, x)) [Life "adam" Nothing Nothing, Life "eve" Nothing Nothing, Life "uranus" Nothing Nothing, Life "gaea" Nothing Nothing, Life "kronos" (Just "gaea") (Just "uranus"), Life "holly" (Just "eve") (Just "adam"), Life "roger" (Just "eve") (Just "kronos"), Life "molly" (Just "holly") (Just "roger"), Life "dolly" (Just "molly") Nothing]

maybeToMonad :: (MonadPlus m) => Maybe a -> m a
maybeToMonad Nothing = mzero
maybeToMonad (Just a) = return a

-- |
--
-- >>> let maybeEgo = (ego lifeDB) :: String -> Maybe Life
-- >>> maybeEgo "deafbeef"
-- Nothing
-- >>> maybeEgo "adam"
-- Just (Life {name = "adam", mother = Nothing, father = Nothing})
-- >>> let listEgo = (ego lifeDB) :: String -> [Life]
-- >>> listEgo "deadbeef"
-- []
-- >>> listEgo "eve"
-- [Life {name = "eve", mother = Nothing, father = Nothing}]
ego :: (Ord a, MonadPlus m) => Map.Map a Life -> a -> m Life
ego db k =
  case Map.lookup k db of
    Nothing -> mzero
    Just a  -> return a

-- |
--
-- >>> let maybeParent = (parent lifeDB) :: Life -> Maybe Life
-- >>> ego lifeDB "adam" >>= maybeParent
-- Nothing
-- >>> ego lifeDB "holly" >>= maybeParent
-- Just (Life {name = "eve", mother = Nothing, father = Nothing})
parent :: (MonadPlus m) => Map.Map String Life -> Life -> m Life
parent db m =
  let
    egoDB = ego db
  in
    (maybeToMonad (mother m) >>= egoDB) `mplus`
    (maybeToMonad (father m) >>= egoDB)

-- |
--
-- >>> let maybeEgo = (ego lifeDB) :: String -> Maybe Life
-- >>> maybeEgo "holly" >>= parentIn lifeDB 1
-- Just (Life {name = "eve", mother = Nothing, father = Nothing})
-- >>> maybeEgo "roger" >>= parentIn lifeDB 2
-- Just (Life {name = "gaea", mother = Nothing, father = Nothing})
-- >>> maybeEgo "molly" >>= parentIn lifeDB 2
-- Just (Life {name = "eve", mother = Nothing, father = Nothing})
-- >>> maybeEgo "molly" >>= parentIn lifeDB 3
-- Just (Life {name = "gaea", mother = Nothing, father = Nothing})
-- >>> maybeEgo "dolly" >>= parentIn lifeDB 3
-- Just (Life {name = "eve", mother = Nothing, father = Nothing})
-- >>> maybeEgo "dolly" >>= parentIn lifeDB 4
-- Just (Life {name = "gaea", mother = Nothing, father = Nothing})
-- >>> let listEgo = (ego lifeDB) :: String -> [Life]
-- >>> listEgo "holly" >>= parentIn lifeDB 1
-- [Life {name = "eve", mother = Nothing, father = Nothing},Life {name = "adam", mother = Nothing, father = Nothing}]
-- >>> listEgo "roger" >>= parentIn lifeDB 2
-- [Life {name = "gaea", mother = Nothing, father = Nothing},Life {name = "uranus", mother = Nothing, father = Nothing}]
-- >>> map (name) $ listEgo "molly" >>= parentIn lifeDB 2
-- ["eve","adam","eve","kronos"]
-- >>> map (name) $ listEgo "molly" >>= parentIn lifeDB 3
-- ["gaea","uranus"]
-- >>> map (name) $ listEgo "dolly" >>= parentIn lifeDB 3
-- ["eve","adam","eve","kronos"]
-- >>> map (name) $ listEgo "dolly" >>= parentIn lifeDB 4
-- ["gaea","uranus"]
parentIn :: (MonadPlus m) => Map.Map String Life -> Int -> Life -> m Life
parentIn db 0 m = return m
parentIn db n m =
  (maybeToMonad (mother m) >>= ego db >>= parentIn db (n - 1)) `mplus`
  (maybeToMonad (father m) >>= ego db >>= parentIn db (n - 1))
