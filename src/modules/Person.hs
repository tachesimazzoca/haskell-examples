{-
  * Person

    ghci> tellPerson (Person "Foo" "Bar" 12)
    "I'm Foo Bar, 12 years old."
    ghci> tellPerson (Person { firstName = "Fuga", lastName = "Baz", age = 23 })
    "I'm Fuga Baz, 23 years old."

    ghci> let p1 = Person "Hoge" "Piyo" 34
    ghci> p1
    Person {firstName = "Hoge", lastName = "Piyo", age = 34}
    ghci> show p1
    "Person {firstName = \"Hoge\", lastName = \"Piyo\", age = 34}"
    ghci> let p2 = read "Person {firstName = \"Hoge\", lastName = \"Piyo\", age = 34}" :: Person
    ghci> p1 == p2
    True
-}

module Person (
  Person(..)
, tellPerson
) where

data Person = Person {
  firstName :: String
, lastName :: String
, age :: Int
} deriving (Eq, Show, Read)

tellPerson :: Person -> String
tellPerson (Person {
  firstName = firstName
, lastName = lastName
, age = age }) =
    "I'm " ++ firstName ++ " " ++ lastName ++ ", " ++ (show age) ++ " years old."
