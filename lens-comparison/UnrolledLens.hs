{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables #-}
module UnrolledLens (unrolledLens) where

import Data.Vinyl.Field
import Data.Vinyl.Rec


class Implicit p where
  implicitly :: p
  
data Elem :: k -> [k] -> * where
  Here  :: Elem x (x ': xs)
  There :: () -> Elem x xs -> Elem x (y ': xs)
type IElem x xs = Implicit (Elem x xs)

instance Implicit (Elem x (x ': xs)) where
  implicitly = Here
instance Implicit (Elem x xs) => Implicit (Elem x (y ': xs)) where
  implicitly = There () implicitly

unrolledLens :: forall r rs sy t f g. (r ~ (sy:::t), IElem r rs, Functor g)
       => r -> (f t -> g (f t)) -> Rec rs f -> g (Rec rs f)
unrolledLens _ = (unrolledLens_unroll :: () -> Elem r rr -> (f t -> g (f t)) -> Rec rr f -> g (Rec rr f)) () implicitly
{-# INLINE unrolledLens #-}

unrolledLensE :: forall r rs sy t f g. (r ~ (sy:::t), Functor g)
       => Elem r rs -> (f t -> g (f t)) -> Rec rs f -> g (Rec rs f)
unrolledLensE = unrolledLens_unroll ()
{-# INLINE unrolledLensE #-}

unrolledLens_unroll :: (r ~ (sy ::: t), Functor g) => () -> Elem r rr -> (f t -> g (f t)) -> Rec rr f -> g (Rec rr f)
unrolledLens_unroll _ = unrolledLens_cont unrolledLens_unroll
{-# NOINLINE unrolledLens_unroll #-} -- Inlining would cause the RULE not to fire

unrolledLens_cont :: (r ~ (sy ::: t), Functor g) 
    => (forall rr'. () -> Elem r rr' -> (f t -> g (f t)) -> Rec rr' f -> g (Rec rr' f))
    -> Elem r rr -> (f t -> g (f t)) -> Rec rr f -> g (Rec rr f)
unrolledLens_cont _    Here        f (x :& xs) = fmap (:& xs) (f x)
unrolledLens_cont cont (There u p) f (x :& xs) = fmap (x :&) (cont u p f xs)
unrolledLens_cont _    Here        _ _         = error "GHC bug #3927"
unrolledLens_cont _    (There _ _) _ _         = error "GHC bug #3927"
{-# INLINE unrolledLens_cont #-}

{-# RULES 
"unrolledLens_unroll" unrolledLens_unroll () = unrolledLens_cont unrolledLens_unroll
    #-}
