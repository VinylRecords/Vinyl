{-# language CPP #-}
import Test.DocTest

main :: IO ()
main = doctest [ "-package lens"
               , "-package doctest"
#if __GLASGOW_HASKELL__ >= 900            
               , "-package singletons-th"
#else
               , "-package singletons"
#endif
               , "tests/Intro.lhs"
               , "Data/Vinyl/Functor.hs"
               , "Data/Vinyl/Curry.hs" ]
