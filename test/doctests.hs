module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "src/modules/Example/Day.hs"
               , "src/modules/Example/Person.hs"
               , "src/modules/Example/Vector.hs"
               , "src/modules/Example/Shapes.hs"
               , "src/modules/Example/HTML.hs" ]
