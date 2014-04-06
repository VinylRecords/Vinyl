module Data.Vinyl.Lift where

newtype Lift op f g x = Lift { runLift :: op (f x) (g x) }
