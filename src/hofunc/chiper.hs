{- 
  * Caesar Chiper

    ghci> encrypt 2 "abc"
    "cde"
    ghci> decrypt 2 "cde"
    "abc"
    ghci> encrypt 16 "Haskell"
    "Xq\131{u||"
    ghci> decrypt 16 "Xq\131{u||"
    "Haskell"
-}

import Data.Char

encrypt :: Int -> String -> String
encrypt n xs = map (chr . (+ n) . ord) xs

decrypt :: Int -> String -> String
decrypt n xs = encrypt (negate n) xs
