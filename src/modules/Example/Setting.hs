module Example.Setting (
  SettingValue(..)
, Setting(..)
, SettingKey
, SettingItem
, insert 
, lookup 
, boolValue
, intValue
, stringValue
) where

import Prelude hiding (lookup)

type SettingKey = String

data SettingValue
  = BoolValue Bool
  | IntValue Int
  | StringValue String
  deriving (Show)

type SettingItem = (SettingKey, SettingValue)

data Setting
  = Empty
  | Node SettingItem Setting Setting
  deriving (Show)

-- |
--
-- >>> insert ("Foo", StringValue "Fuga") Empty
-- Node ("Foo",StringValue "Fuga") Empty Empty
-- >>> foldr insert Empty [("1", IntValue 1), ("2", BoolValue False)]
-- Node ("2",BoolValue False) (Node ("1",IntValue 1) Empty Empty) Empty
insert :: SettingItem -> Setting -> Setting
insert a Empty = Node a Empty Empty
insert (ak, va) (Node (bk, bv) l r)
  | ak < bk   = Node (bk, bv) (insert (ak, va) l) r
  | ak > bk   = Node (bk, bv) l (insert (ak, va) r)
  | otherwise = Node (ak, va) l r

-- |
--
-- >>> lookup "Foo" Empty
-- Nothing
-- >>> let setting = (Node ("Foo", (BoolValue True)) Empty Empty)
-- >>> lookup "Foo" setting
-- Just (BoolValue True)
-- >>> lookup "Bar" setting
-- Nothing
-- >>> lookup "Bar" (Node ("Foo", (BoolValue True)) (Node ("Bar", (IntValue 123)) Empty Empty) Empty)
-- Just (IntValue 123)
lookup :: SettingKey -> Setting -> Maybe SettingValue
lookup _ Empty = Nothing
lookup a (Node (b, v) l r)
  | a < b     = lookup a l
  | a > b     = lookup a r
  | otherwise = Just v

-- |
--
-- >>> boolValue (BoolValue True)
-- True
-- >>> boolValue (IntValue 0)
-- False
-- >>> boolValue (IntValue 1)
-- True
-- >>> boolValue (StringValue "")
-- False
-- >>> boolValue (StringValue "0")
-- True
boolValue :: SettingValue -> Bool
boolValue (BoolValue v)   = v
boolValue (IntValue v)    = v /= 0
boolValue (StringValue v) = not (null v)

-- |
--
-- >>> intValue (BoolValue True)
-- 1
-- >>> intValue (BoolValue False)
-- 0
-- >>> intValue (IntValue 0)
-- 0
-- >>> intValue (IntValue 1)
-- 1
-- >>> intValue (StringValue "")
-- 0
-- >>> intValue (StringValue "1")
-- 0
intValue :: SettingValue -> Int
intValue (BoolValue v)   = if v then 1 else 0
intValue (IntValue v)    = v
intValue _               = 0

-- |
--
-- >>> stringValue (BoolValue True)
-- "1"
-- >>> stringValue (BoolValue False)
-- "0"
-- >>> stringValue (IntValue 0)
-- "0"
-- >>> stringValue (IntValue 123)
-- "123"
-- >>> stringValue (StringValue "")
-- ""
-- >>> stringValue (StringValue "Foo")
-- "Foo"
stringValue :: SettingValue -> String
stringValue (BoolValue v)   = if v then "1" else "0"
stringValue (IntValue v)    = show v
stringValue (StringValue v) = v
