{-
  * Lookup

    ghci> lookup' "bar" [("foo", "foo@example.net"), ("bar", "bar@example.net")]
    -- if ("foo" == "bar") then (Just "foo@example.net") else (lookup' "bar" [("bar", "bar@example.net")])
    -- if ("bar" == "bar") then (Just "bar@example.net") else (lookup' "bar" [])
    ghci> Just "bar@example.net"

    ghci> lookup' "baz" [("foo", "foo@example.net"), ("bar", "bar@example.net")]
    -- if ("foo" == "baz") then (Just "foo@example.net") else (lookup' "bar" [("bar", "bar@example.net")])
    -- if ("bar" == "baz") then (Just "bar@example.net") else (lookup' "bar" [])
    ghci> Nothing
-}

lookup' :: (Eq k) => k -> [(k, v)] -> Maybe v
lookup' _ [] = Nothing
lookup' key ((k, v):xs)
  | k == key = Just v
  | otherwise = lookup' key xs
