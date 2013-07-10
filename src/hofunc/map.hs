{-
  * Map

    map' (+ 1) [1..3]
    -- 1 + 1 : map' (+ 1) [2..3]
    -- 1 + 1 : (2 + 1 : map' (+ 1) [3])
    -- 1 + 1 : (2 + 1 : (3 + 1 : map' (+ 1) []))
    -- 1 + 1 : (2 + 1 : (3 + 1 : []))
    [2, 3, 4]
    
    map' (max 0) [-1..1]
    -- (max 0 -1) : map' (max 0) [0..1]
    -- (max 0 -1) : ((max 0 0) : map' (max 0) [1])
    -- (max 0 -1) : ((max 0 0) : ((max 1 0) : map' (max 0) []))
    -- (max 0 -1) : ((max 0 0) : ((max 1 0) : []))
    [0, 0, 1]
    
    map' (take 1) ["Foo", "Bar"]
    -- (take 1 "Foo") : map' (take 1) "Bar"
    -- (take 1 "Foo") : ((take 1 "Bar"), map' (take 1) [])
    -- (take 1 "Foo") : ((take 1 "Bar"), [])
    ["F", "B"]

    ghci> (listOfNumbering !! 1) "Foo"
    "1. Foo"
    ghci> (listOfNumbering !! 2) "Bar"
    "2. Bar"
    ghci> (listOfDescribing !! 1) "Foo"
    "Foo is the 1st"
    ghci> (listOfDescribing !! 2) "Bar"
    "Bar is the 2nd"
-}

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

listOfNumbering = map (++) ["", "1. ", "2. ", "3. "]
listOfDescribing = map (flip (++)) ["", " is the 1st", " is the 2nd", " is the 3rd"]
