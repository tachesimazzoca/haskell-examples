{-
  * Fold

    ghci> join "," ["Foo", "Bar", "Baz"]
    "Foo,Bar,Baz"

    ghci> lookupr' "bar" [("foo", "foo-1"), ("bar", "bar-1"), ("foo", "foo-2"), ("bar", "bar-2")]
    -- if "bar" == "bar" then Just "bar-2" else Nothing
    -- if "foo" == "bar" then Just "foo-2" else Just "bar-2"
    -- if "bar" == "bar" then Just "bar-1" else Just "bar-2"
    -- if "foo" == "bar" then Just "foo-1" else Just "bar-1"
    Just "bar-1"

    ghci> lookupl' "bar" [("foo", "foo-1"), ("bar", "bar-1"), ("foo", "foo-2"), ("bar", "bar-2")]
    -- if "foo" == "bar" then Just "foo-1" else Nothing
    -- if "bar" == "bar" then Just "bar-1" else Nothing
    -- if "bar" == "foo" then Just "foo-2" else Just "bar-1"
    -- if "bar" == "bar" then Just "bar-2" else Just "bar-1"
    Just "bar-2"
-}

join :: String -> [String] -> String
join sep = foldl f ""
  where f "" x = x
        f acc x = acc ++ sep ++ x

lookupr' :: (Eq k) => k -> [(k, v)] -> Maybe v
lookupr' key xs = foldr (\(k, v) acc -> if k == key then Just v else acc) Nothing xs

lookupl' :: (Eq k) => k -> [(k, v)] -> Maybe v
lookupl' key xs = foldl (\acc (k, v) -> if k == key then Just v else acc) Nothing xs
