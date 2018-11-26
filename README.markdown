Vinyl is a general solution to the records problem in Haskell using type level strings and other modern GHC features, featuring static structural typing (with a subtyping relation), and automatic row-polymorphic lenses. All this is possible without Template Haskell.

First, install Vinyl from [Hackage](http://hackage.haskell.org/package/vinyl):

    cabal update
    cabal install vinyl

To learn more, [try this tutorial](https://github.com/VinylRecords/Vinyl/blob/master/tests/Intro.lhs).

## Performance Scaling
Vinyl is an approach to records in Haskell based on a heterogeneous list type: a list where each element may have a different type. Since it is a list, accessing any individual field of a Vinyl record seems as though it should have *O(n)* complexity, with *n* the size of the record. Sure enough, pattern matching on a Vinyl record works just as with a list: we peel off the head element until we reach the one we are looking for. But we have an option to change these asymptotics.

If we anticipate frequent `get` operations on a record, we can use the function `toARec :: Rec f ts -> ARec f ts` to produce an *array-backed `Rec`* that offers constant-time field access. We can consider a [benchmark](https://github.com/VinylRecords/Vinyl/blob/master/benchmarks/AccessorsBench.hs) of getting specific fields from different varieties of Vinyl records.

This benchmark suggests the trends we should expect

* Getting a field deeper in a Vinyl record takes more time than getting one earlier (i.e. it appears to the left, or under fewer `:&` constructors) in the record
* Getting a field of an `ARec` (or `AFieldRec`, which is an `ARec` with named fields) array-backed record does not depend on where in the record that field lies

The x-axis of this graph should be labeled as *nanoseconds*.

![Accessors benchmark plot](/images/accessors.png)
