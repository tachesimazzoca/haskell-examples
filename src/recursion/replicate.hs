{-
  * Replicate

    ghci> replicate' 3, "Foo"
    -- "Foo" : replicate' 2 "Foo"
    -- "Foo" : ("Foo" : replicate' 1 "Foo")
    -- "Foo" : ("Foo" : ("Foo" : replicate' 0 "Foo"))
    -- "Foo" : ("Foo" : ("Foo" : []))
    ["Foo", "Foo", Foo"]
-}

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x
