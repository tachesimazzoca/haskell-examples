{-
  * Repeat

  Use `take` to get a list, for example `take 5 (repeat' "Foo")`.

    ghci> repeat' "Foo"
    -- "Foo" : repeat' "Foo"
    -- "Foo" : ("Foo" : repeat' "Foo")
    -- "Foo" : ("Foo" : ("Foo" : repeat' "Foo"))
    -- "Foo" : ("Foo" : ("Foo" : ("Foo" : repeat' "Foo")))
    --   .... will never finish evaluating.
-}

repeat' :: a -> [a]
repeat' x = x : repeat' x
