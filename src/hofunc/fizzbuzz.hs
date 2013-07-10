{- 
  * FizzBuzz

    ghci> fizzbuzz [1..100]
    ["1","2","fizz","4","buzz","fizz","7","8","fizz","buzz","11","fizz","13","14","fizzbuzz",....
-}

fizzbuzz :: [Int] -> [String]
fizzbuzz =
  let f x
        | fizz && buzz = "fizzbuzz"
        | fizz = "fizz"
        | buzz = "buzz"
        | otherwise = show x
        where fizz = x `mod` 3 == 0
              buzz = x `mod` 5 == 0
  in map f
