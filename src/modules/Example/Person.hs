module Example.Person (
  Person(..)
, tellPerson
) where

-- |
--
-- >>> let p1 = Person "Hoge" "Piyo" 34
-- >>> p1
-- Person {firstName = "Hoge", lastName = "Piyo", age = 34}
-- >>> show p1
-- "Person {firstName = \"Hoge\", lastName = \"Piyo\", age = 34}"
-- >>> let p2 = read "Person {firstName = \"Hoge\", lastName = \"Piyo\", age = 34}" :: Person
-- >>> p1 == p2
-- True
data Person = Person {
  firstName :: String
, lastName :: String
, age :: Int
} deriving (Eq, Show, Read)

-- |
--
-- >>> tellPerson (Person "Foo" "Bar" 12)
-- "I'm Foo Bar, 12 years old."
-- >>> tellPerson (Person { firstName = "Fuga", lastName = "Baz", age = 23 })
-- "I'm Fuga Baz, 23 years old."
tellPerson :: Person -> String
tellPerson (Person {
  firstName = fn
, lastName = ln
, age = a }) =
    "I'm " ++ fn ++ " " ++ ln ++ ", " ++ (show a) ++ " years old."
