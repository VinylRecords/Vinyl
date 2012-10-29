This README is Literate Haskell; to play, run the following:

    ghci README.lhs

GHC 7.6 offers type level kinds for strings and natural numbers; the
former allows a general solution to the records-problem, featuring
static structural typing (with a subtyping relation), and automatic
row-polymorphic lenses. All this is possible without Template Haskell.

Let's work through a quick example. We'll need to enable some language
extensions first:

> {-# LANGUAGE DataKinds, TypeOperators, NoMonomorphismRestriction #-}
> {-# LANGUAGE OverlappingInstances, FlexibleInstances, FlexibleContexts #-}

> import Data.Records
> import Control.Category
> import Prelude hiding (id, (.))


Let's define the fields we want to use:

> name     = Field :: "name"     ::: String
> age      = Field :: "age"      ::: Int
> sleeping = Field :: "sleeping" ::: Bool

Now, let's try to make an entity that represents a man:

> jon = (name, "jon")
>    :& (age, 20)
>    :& (sleeping, False)
>    :& RNil

We could make an alias for the sort of entity that `jon` is:

> type LifeForm = Rec '["name" ::: String, "age" ::: Int, "sleeping" ::: Bool]
> jon :: LifeForm

The types are inferred, though, so this is unnecessary unless you'd like
to reuse the type later. Now, make a dog! Dogs are life-forms, but
unlike men, they have masters. So, let's build my dog:

> master = Field :: "master" ::: LifeForm

> tucker = (name, "tucker")
>       :& (age, 7)
>       :& (sleeping, True)
>       :& (master, jon)
>       :& RNil


Using Lenses
------------

Now, if we want to wake entities up, we don't want to have to write a
separate wake-up function for both dogs and men (even though they are of
different type). Luckily, we can use the built-in lenses to focus on a
particular field in the record for access and update, without losing
additional information:

> wakeUp :: IElem ("sleeping" ::: Bool) fields => Rec fields -> Rec fields
> wakeUp = rPut sleeping False

Now, the type annotation on `wakeUp` was not necessary; I just wanted to
show how intuitive the type is. Basically, it takes as an input any
record that has a `BOOL` field labelled `sleeping`, and the modifies
that specific field in the record accordingly.

> tucker' = wakeUp tucker
> jon' = wakeUp jon

We can also access the entire lens for a field using the `rLens`
function; since lenses are composable, it's super easy to do deep update
on a record:

> masterSleeping :: IElem ("master" ::: LifeForm) fields => Lens (Rec fields) Bool
> masterSleeping = rLens sleeping . rLens master

> tucker'' = put masterSleeping True tucker'

(Again, the type annotation is unnecessary.)


Subtyping Relation and Coercion
-------------------------------

A record `Rec xs` is a subtype of a record `Rec ys` if `ys` is a subset
of `xs`; that is to say, if one record can do everything that another
record can, the former is a subtype of the latter. As such, we should be
able to provide an upcast operator which "forgets" whatever makes one
record different from another (whether it be extra data, or different
order).

Therefore, the following works:

> upcastedTucker :: LifeForm
> upcastedTucker = cast tucker

The subtyping relationship between record types is expressed with the
`(<:)` constraint; so, `cast` is of the following type:

< cast :: ss <: ts => Rc ss -> Rec ts

Also provided is a (:~:) constraint which indicates record congruence
(that is, two record types differ only in the order of their fields).
