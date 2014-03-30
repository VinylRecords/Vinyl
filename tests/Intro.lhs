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

> {-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
> {-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
> {-# LANGUAGE GADTs, TemplateHaskell #-}
> import Data.Vinyl
> import Data.Vinyl.Idiom.Identity
> import Data.Vinyl.Idiom.Validation
> import Control.Applicative
> import Control.Lens hiding (Identity)
> import Data.Char
> import Test.DocTest
> import Data.Singletons.TypeLits
> import Data.Singletons.TH
> import Data.Singletons.Tuple

Let’s define a universe of fields which we want to use:

> $(singletons [d|
>   data Fields = Name | Age | Sleeping | Master deriving Show
>   |])
> type instance El Name = String
> type instance El Age = Int
> type instance El Sleeping = Bool

Now, let’s try to make an entity that represents a man:

> jon = SName =: "jon"
>    <+> SAge =: 20
>    <+> SSleeping =: False


We could make an alias for the sort of entity that jon is:

> type LifeForm = [Name, Age, Sleeping]
> jon :: PlainRec LifeForm

The types are inferred, though, so this is unnecessary unless you’d
like to reuse the type later. Now, make a dog! Dogs are life-forms,
but unlike men, they have masters. So, let’s build my dog:

> type instance El Master = PlainRec LifeForm

> tucker = SName =: "tucker"
>      <+> SAge =: 7
>      <+> SSleeping =: True
>      <+> SMaster =: jon


Using Lenses
------------

Now, if we want to wake entities up, we don’t want to have to write a
separate wake-up function for both dogs and men (even though they are
of different type). Luckily, we can use the built-in lenses to focus
on a particular field in the record for access and update, without
losing additional information:

> wakeUp :: (Sleeping ∈ fields) => PlainRec fields -> PlainRec fields
> wakeUp = SSleeping `rPut` False

Now, the type annotation on wakeUp was not necessary; I just wanted to
show how intuitive the type is. Basically, it takes as an input any
record that has a `Bool` field labelled `sleeping`, and modifies that
specific field in the record accordingly.

> tucker' = wakeUp tucker
> jon' = wakeUp jon

> -- |
> -- >>> tucker' ^. rLens SSleeping
> -- False
> --
> -- >>> tucker ^. rLens SSleeping
> -- True
> --
> -- >>> jon' ^. rLens SSleeping
> -- False

We can also access the entire lens for a field using the rLens
function; since lenses are composable, it’s super easy to do deep
update on a record:

> masterSleeping :: (Master ∈ fields) => Lens' (PlainRec fields) Bool
> masterSleeping = rLens SMaster . rLens SSleeping
> tucker'' = masterSleeping .~ True $ tucker'


> -- | >>> tucker'' ^. masterSleeping
> -- True

Again, the type annotation is unnecessary.


Subtyping Relation and Coercion
-------------------------------

A record `PlainRec xs` is a subtype of a record `PlainRec ys` if `ys ⊆ xs`;
that is to say, if one record can do everything that another record
can, the former is a subtype of the latter. As such, we should be able
to provide an upcast operator which “forgets” whatever makes one
record different from another (whether it be extra data, or different
order).

Therefore, the following works:

> upcastedTucker :: PlainRec LifeForm
> upcastedTucker = cast (toPlainRec tucker)

The reason for using `toPlainRec` will become clear a bit later.

The subtyping relationship between record types is expressed with the
`(<:)` constraint; so, cast is of the following type:

< cast :: r1 <: r2 => Rec r1 f -> Rec r2 f

Also provided is a `(≅)` constraint which indicates record congruence
(that is, two record types differ only in the order of their fields).

Records are polymorphic over functors
-------------------------------------

So far, we’ve been working with the `PlainRec` type; but below that,
there is something a bit more advanced called `Rec`, which looks like
this:

< data Rec :: [k] -> (* -> *) -> * where
<   RNil :: Rec '[] f
<   (:&) :: f (El r) -> Rec rs f -> Rec (r ': rs) f

The second parameter is a functor, in which every element of the
record will be placed. In `PlainRec`, the functor is just set to
`Identity`. Let’s try and motivate this stuff with an example.

Let’s imagine that we want to do validation on a record that
represents a name and an age:

> type Person = [Name, Age]

We’ve decided that names must be alphabetic, and ages must be
positive. For validation, we’ll use a type that’s included here called
`Result e a`, which is similar to `Either`, except that its
`Applicative` instance accumulates monoidal errors on the left.

> goodPerson :: PlainRec Person
> goodPerson = SName =: "Jon"
>          <+> SAge  =: 20
> badPerson = SName =: "J#@#$on"
>         <+> SAge  =: 20

> validatePerson :: PlainRec Person -> Result [String] (PlainRec Person)
> validatePerson p = (\n a -> SName =: n <+> SAge =: a) <$> vName <*> vAge where
>   vName = validateName (rGet SName p)
>   vAge  = validateAge  (rGet SAge p)
>
>   validateName str | all isAlpha str = Success str
>   validateName _ = Failure [ "name must be alphabetic" ]
>   validateAge i | i >= 0 = Success i
>   validateAge _ = Failure [ "age must be positive" ]

> -- $setup
> -- >>> let isSuccess (Success _) = True; isSuccess _ = False

> -- |
> -- >>> isSuccess $ validatePerson goodPerson
> -- True
> --
> -- >>> isSuccess $ validatePerson badPerson
> -- False

The results are as expected (`Success` for `goodPerson`, and a
`Failure` with one error for `badPerson`); but this was not very fun
to build.

Further, it would be nice to have some notion of a partial record;
that is, if part of it can’t be validated, it would still be nice to
be able to access the rest. What if we could make a version of this
record where the elements themselves were validation functions, and
then that record could be applied to a plain one, to get a record of
validated fields? That’s what we’re going to do.

Vinyl provides a type of validators, which is basically a natural
transformation from the `Identity` functor to the `Result` functor, which
we just used above.

< type Validator e = Identity ~> Result e

Let’s parameterize a record by it: when we do, then an element of type
`a` should be a function `Identity a -> Result e a`:

> vperson :: Rec Person (Validator [String])
> vperson = NT validateName :& NT validateAge :& RNil where
>    validateName (Identity str) | all isAlpha str = Success str
>    validateName _ = Failure [ "name must be alphabetic" ]
>    validateAge (Identity i) | i >= 0 = Success i
>    validateAge _ = Failure [ "age must be positive" ]


And we can use the special application operator `<<*>>` (which is
analogous to `<*>`, but generalized a bit) to use this to validate a
record:

> goodPersonResult = vperson <<*>> goodPerson
> badPersonResult  = vperson <<*>> badPerson

< goodPersonResult === SName :=: Success "Jon", SAge :=: Success 20, {}
< badPersonResult  === SName :=: Failure ["name must be alphabetic"], SAge :=: Success 20, {}

> -- |
> -- >>> isSuccess $ goodPersonResult ^. rLens' SName
> -- True
> -- >>> isSuccess $ goodPersonResult ^. rLens' SAge
> -- True
> -- >>> isSuccess $ badPersonResult ^. rLens' SName
> -- False
> -- >>> isSuccess $ badPersonResult ^. rLens' SAge
> -- True

So now we have a partial record, and we can still do stuff with its
contents. Next, we can even recover the original behavior of the
validator (that is, to give us a value of type `Result [String]
(PlainRec Person)`) using `rdist`:

> distGoodPerson = rdist goodPersonResult
> distBadPerson  = rdist badPersonResult

< distGoodPerson === Success name :=: "Jon", age :=: 20, {}
< distBadPerson  === Failure ["name must be alphabetic"]

> -- |
> -- >>> isSuccess distGoodPerson
> -- True
> -- >>> isSuccess distBadPerson
> -- False

Fixing a polymorphic record into the Identity Functor
-----------------------------------------------------

If you produced a record using `(=:)` and `(<+>)` without providing a
type annotation, then its type is something like this:

< record :: Applicative f => Record [ <bunch of stuff> ] f

The problem is then we can’t do anything with the record that requires
us to know what its functor is. For instance, `cast` will fail. So, we
might try to provide a type annotation, but that can be a bit brittle
and frustrating to have to do. To alleviate this problem, `toPlainRec` is
provided:

< toPlainRec :: (forall f. Applicative f => Rec rs f) -> PlainRec rs

---

(We must define a main value for doctest to run.)

> main :: IO ()
> main = doctest ["tests/Intro.lhs"]

