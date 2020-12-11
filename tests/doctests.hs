import Test.DocTest

main :: IO ()
main = doctest [ "-package lens"
               , "-package doctest"
               , "-package singletons"
               , "tests/Intro.lhs"
               , "Data/Vinyl/Functor.hs"
               , "Data/Vinyl/Curry.hs" ]
