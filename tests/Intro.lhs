This introduction was originally published at
<http://www.jonmsterling.com/posts/2013-04-06-vinyl-modern-records-for-haskell.html>

Vinyl: Modern Records for Haskell
=================================

Vinyl is a general solution to the records problem in Haskell using
type level strings and other modern GHC features, featuring static
structural typing (with a subtyping relation), and automatic
row-polymorphic lenses. All this is possible without Template Haskell.

First, install Vinyl from Hackage:

< cabal update
< cabal install vinyl

Let’s work through a quick example. We’ll need to enable some language
extensions first:

> {-# LANGUAGE DataKinds, PolyKinds, TypeOperators, TypeFamilies #-}
> {-# LANGUAGE FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}
> {-# LANGUAGE GADTs, TypeSynonymInstances, TemplateHaskell, StandaloneDeriving #-}
> import Data.Vinyl
> import Data.Vinyl.Functor
> import Control.Applicative
> import Control.Lens hiding (Identity)
> import Control.Lens.TH
> import Data.Char
> import Test.DocTest
> import Data.Singletons.TH

Let’s define a universe of fields which we want to use:

> data Fields = Name | Age | Sleeping | Master deriving Show
> type LifeForm = [Name, Age, Sleeping]

> type family ElF (f :: Fields) :: * where
>   ElF Name = String
>   ElF Age = Int
>   ElF Sleeping = Bool
>   ElF Master = Rec Attr LifeForm

> newtype Attr f = Attr { _unAttr :: ElF f }
> makeLenses ''Attr
> instance Show (Attr Name) where show (Attr x) = "name: " ++ show x
> instance Show (Attr Age) where show (Attr x) = "age: " ++ show x
> instance Show (Attr Sleeping) where show (Attr x) = "sleeping: " ++ show x
> instance Show (Attr Master) where show (Attr x) = "master: " ++ show x

> (=:) :: sing f -> ElF f -> Attr f
> _ =: x = Attr x

> genSingletons [ ''Fields ]

Now, let’s try to make an entity that represents a man:

> jon = (SName =: "jon")
>    :& (SAge =: 23)
>    :& (SSleeping =: False)
>    :& RNil

Automatically, we can show the record:

> -- |
> -- >>> show jon
> -- "{name: \"jon\"; age: 23; sleeping: False}"

And its types are all inferred with no problem. Now, make a dog! Dogs are
life-forms, but unlike men, they have masters. So, let’s build my dog:

> tucker = (SName =: "tucker")
>       :& (SAge =: 9)
>       :& (SSleeping =: True)
>       :& (SMaster =: jon)
>       :& RNil

Using Lenses
------------

Now, if we want to wake entities up, we don’t want to have to write a
separate wake-up function for both dogs and men (even though they are
of different type). Luckily, we can use the built-in lenses to focus
on a particular field in the record for access and update, without
losing additional information:


> wakeUp :: (Sleeping ∈ fields) => Rec Attr fields -> Rec Attr fields
> wakeUp = rput $ SSleeping =: False

Now, the type annotation on wakeUp was not necessary; I just wanted to
show how intuitive the type is. Basically, it takes as an input any
record that has a `Bool` field labelled `sleeping`, and modifies that
specific field in the record accordingly.

> tucker' = wakeUp tucker
> jon' = wakeUp jon

> -- |
> -- >>> tucker' ^. rlens SSleeping
> -- sleeping: False
> --
> -- >>> tucker ^. rlens SSleeping
> -- sleeping: True
> --
> -- >>> jon' ^. rlens SSleeping
> -- sleeping: False

We can also access the entire lens for a field using the rLens
function; since lenses are composable, it’s super easy to do deep
update on a record:

> masterSleeping = rlens SMaster . unAttr . rlens SSleeping
> tucker'' = masterSleeping .~ (SSleeping =: True) $ tucker'

> -- | >>> tucker'' ^. masterSleeping
> -- sleeping: True

Subtyping Relation and Coercion
-------------------------------

A record `Rec f xs` is a subtype of a record `Rec f ys` if `ys ⊆ xs`;
that is to say, if one record can do everything that another record
can, the former is a subtype of the latter. As such, we should be able
to provide an upcast operator which “forgets” whatever makes one
record different from another (whether it be extra data, or different
order).

Therefore, the following works:

> upcastedTucker :: Rec Attr LifeForm
> upcastedTucker = rcast tucker

The subtyping relationship between record types is expressed with the
`(<:)` constraint; so, cast is of the following type:

< rcast :: r1 <: r2 => Rec f r1 -> Rec f r2

Also provided is a `(≅)` constraint which indicates record congruence
(that is, two record types differ only in the order of their fields).

In fact, `rcast` is actually given as a special case of the lens `rsubset`,
which lets you modify entire (possibly non-contiguous) slices of a record!

Records are polymorphic over functors
-------------------------------------

Consider the following declaration:

< data Rec :: (u -> *) -> [u] -> * where
<   RNil :: Rec f '[]
<   (:&) :: f r -> Rec f rs -> Rec f (r ': rs)

Records are implicitly parameterized over a kind `u`, which stands for the
"universe" or key space. Keys (inhabitants of `u`) are then interpreted into
the types of their values by the first parameter to `Rec`, `f`. An extremely
powerful aspect of Vinyl records is that you can construct natural
transformations between different interpretation functors `f,g`, or postcompose
some other functor onto the stack. This can be used to immerse each field of a
record in some particular effect modality, and then the library functions can
be used to traverse and accumulate these effects.

Let’s imagine that we want to do validation on a record that
represents a name and an age:

> type Person = [Name, Age]

We’ve decided that names must be alphabetic, and ages must be positive. For
validation, we’ll use `Maybe` for now, though you should use a
left-accumulating `Validation` type.

> goodPerson :: Rec Attr Person
> goodPerson = (SName =: "Jon")
>           :& (SAge =: 20)
>           :& RNil

> badPerson = (SName =: "J#@#$on")
>           :& (SAge =: 20)
>           :& RNil

We'll give validation a (rather poor) shot.

> validatePerson :: Rec Attr Person -> Maybe (Rec Attr Person)
> validatePerson p = (\n a -> (SName =: n) :& (SAge =: a) :& RNil) <$> vName <*> vAge where
>   vName = validateName $ p ^. rlens SName . unAttr
>   vAge  = validateAge $ p ^. rlens SAge . unAttr
>
>   validateName str | all isAlpha str = Just str
>   validateName _ = Nothing
>   validateAge i | i >= 0 = Just i
>   validateAge _ = Nothing

> -- $setup
> -- >>> let isJust (Just _) = True; isJust _ = False

> -- |
> -- >>> isJust $ validatePerson goodPerson
> -- True
> --
> -- >>> isJust $ validatePerson badPerson
> -- False

The results are as expected (`Just` for `goodPerson`, and a `Nothing` for
`badPerson`); but this was not very fun to build.

Further, it would be nice to have some notion of a partial record;
that is, if part of it can’t be validated, it would still be nice to
be able to access the rest. What if we could make a version of this
record where the elements themselves were validation functions, and
then that record could be applied to a plain one, to get a record of
validated fields? That’s what we’re going to do.

> type Validator f = Lift (->) f (Maybe :. f)

Let’s parameterize a record by it: when we do, then an element of type
`a` should be a function `Identity a -> Result e a`:

> vperson :: Rec (Validator Attr) Person
> vperson = lift validateName :& lift validateAge :& RNil
>   where
>     lift f = Lift $ Compose . f
>     validateName (Attr str) | all isAlpha str = Just (Attr str)
>     validateName _ = Nothing
>     validateAge (Attr i) | i >= 0 = Just (Attr i)
>     validateAge _ = Nothing

And we can use the special application operator `<<*>>` (which is
analogous to `<*>`, but generalized a bit) to use this to validate a
record:

> goodPersonResult = vperson <<*>> goodPerson
> badPersonResult  = vperson <<*>> badPerson

> -- |
> -- >>> isJust . getCompose $ goodPersonResult ^. rlens SName
> -- True
> -- >>> isJust . getCompose $ goodPersonResult ^. rlens SAge
> -- True
> -- >>> isJust . getCompose $ badPersonResult ^. rlens SName
> -- False
> -- >>> isJust . getCompose $ badPersonResult ^. rlens SAge
> -- True


So now we have a partial record, and we can still do stuff with its contents.
Next, we can even recover the original behavior of the validator (that is, to
give us a value of type `Maybe (Rec Attr Person)`) using `rtraverse`:

> mgoodPerson :: Maybe (Rec Attr Person)
> mgoodPerson = rtraverse getCompose goodPersonResult

> mbadPerson  = rtraverse getCompose badPersonResult

> -- |
> -- >>> isJust mgoodPerson
> -- True
> -- >>> isJust mbadPerson
> -- False

> main :: IO ()
> main = doctest ["tests/Intro.lhs"]
