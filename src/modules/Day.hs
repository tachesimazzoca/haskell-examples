{-
  * Day

    -- Eq
    ghci> Monday == Monday
    True
    -- Ord
    ghci> Monday > Tuesday
    False
    -- Read
    ghci> show Monday
    "Monday"
    -- Read
    ghci> read "Monday" :: Week
    Monday
    -- Bounded
    ghci> minBound :: Week
    Monday
    ghci> maxBound :: Week
    Sunday
    -- Enum
    ghci> [Monday .. Sunday]
    [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]
-}

module Day (
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
