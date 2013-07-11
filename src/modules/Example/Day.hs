-- |
--
-- >>> Monday == Monday
-- True
-- >>> Monday > Tuesday
-- False
-- >>> show Monday
-- "Monday"
-- >>> read "Monday" :: Week
-- Monday
-- >>> minBound :: Week
-- Monday
-- >>> maxBound :: Week
-- Sunday
-- >>> [Monday .. Sunday]
-- [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]

module Example.Day (
  Week(..)
) where

data Week
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)
