import Test.DocTest

main :: IO ()
main = doctest [ "tests/Intro.lhs"
               , "Data/Vinyl/Functor.hs"
               , "Data/Vinyl/Curry.hs" ]
