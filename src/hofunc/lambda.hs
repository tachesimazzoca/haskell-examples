{-
  * Lambda

    ghci> surroundWith "<hs:" " />" ["foo", "bar"]
    ["<hs:foo />", "<hs:bar />"]
    ghci> surroundWith "[" "]" (map show [0..9])
    ["[0]","[1]","[2]","[3]","[4]","[5]","[6]","[7]","[8]","[9]"]
-}

surroundWith :: String -> String -> ([String] -> [String])
surroundWith l r = map (\x -> l ++ x ++ r)
