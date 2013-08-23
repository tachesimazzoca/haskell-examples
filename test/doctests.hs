module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "src/modules/Example/Day.hs"
               , "src/modules/Example/Person.hs"
               , "src/modules/Example/Vector.hs"
               , "src/modules/Example/Shapes.hs"
               , "src/modules/Example/HTML.hs"
               , "src/modules/Example/Validator.hs"
               , "src/modules/Example/Tree.hs"
               , "src/modules/Example/Setting.hs"
               , "src/modules/Example/Recursion.hs"
               , "src/modules/Example/HigherOrderFunction.hs"
               , "src/modules/Example/Road.hs"
               , "src/modules/Example/Applicative.hs"
               , "src/modules/Example/Monoid.hs"
               , "src/modules/Example/Monad.hs"
               , "src/modules/Example/MonadPlus.hs"
               , "src/modules/Example/Writer.hs"
               , "src/modules/Example/State.hs"
               , "src/modules/Example/Dice.hs"
               , "src/modules/Example/Sheep.hs"
               , "src/modules/Example/Calc.hs"
               , "src/modules/Example/Kinship.hs"
               , "src/modules/Example/Parser.hs"
               , "src/modules/Example/Poker.hs" ]
