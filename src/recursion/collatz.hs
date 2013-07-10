{-
  * Collatz conjecture

  <http://en.wikipedia.org/wiki/Collatz_conjecture>

    ghci> chain 10
    -- 10 : chain 5
    -- 10 : (5 : chain 16)
    -- 10 : (5 : (16 : chain 8))
    -- 10 : (5 : (16 : (8 : chain 4)))
    -- 10 : (5 : (16 : (8 : (4 : chain 2))))
    -- 10 : (5 : (16 : (8 : (4 : (2 : chain 1)))))
    -- 10 : (5 : (16 : (8 : (4 : (2 : [1])))))
    [10, 5, 16, 8, 4, 2, 1]
-} 

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x
  | even x = x : chain (x `div` 2)
  | odd x = x : chain (x * 3 + 1)
