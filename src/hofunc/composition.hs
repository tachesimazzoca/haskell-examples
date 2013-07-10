{-
  * Function Composition

    ghci> discount 10000
    9000
    ghci> addTax (discount 10000)
    9450
    ghci> useCoupon (addTax (discount 10000))
    8450
    ghci> addShippingCharge (useCoupon (addTax (discount 10000)))
    8765
    ghci> (addShippingCharge . useCoupon . addTax . discount) 10000
    8765
    ghci> (foldl (.) (+ 0) (reverse [discount, addTax, useCoupon, addShippingCharge])) 10000
    8765
    ghci> (foldl (flip (.)) (+ 0) [discount, addTax, useCoupon, addShippingCharge]) 10000
    8765

    ghci> payment 10000 [discount]
    9000
    ghci> payment 10000 [discount, addTax]
    9450
    ghci> payment 10000 [discount, addTax, useCoupon]
    8450
    ghci> payment 10000 [discount, addTax, useCoupon, addShippingCharge]
    8765
    ghci> payment 10000 [rate 0.9, rate 1.05, subtract 1000, (+ rate 1.05 300)]
    8765
-}

rate :: Double -> Int -> Int
rate x y = floor((fromIntegral(y)::Double) * x)

discount :: Int -> Int
discount = rate 0.90

addTax :: Int -> Int
addTax = rate 1.05

addShippingCharge :: Int -> Int
addShippingCharge = (+ (addTax 300))

useCoupon :: Int -> Int
useCoupon x = max 0 (x - 1000)

payment :: Int -> [(Int -> Int)] -> Int
payment p xs = (foldl (flip (.)) (+ 0) xs) p
