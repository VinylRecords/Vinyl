# 0.10.0

- Changed the types of `Data.Vinyl.CoRec.onCoRec` and `Data.Vinyl.CoRec.onField`. This was pushing through the changes to drop the use of `Proxy` arguments, relying instead on `TypeApplications`. Also added `onCoRec1` and `onField` to work with functions relying on a single type class.

- Faster `asA` and `asA'`. These implementations utilize `unsafeCoerce` in their implementations after we have performed a runtime check that proves (to us) that the types match up. The old implementations are still available as `asASafe` and `asA'Safe`. While both implementations can run in constant time if the compiler optimizes everything successfully, the faster variants are a bit more than 3x faster in a trivial benchmark.

# 0.9.2

- Add `runcurryX` for applying an uncurried function to a `Rec` passing through the `XRec` machinery to strip out syntactic noise.

# 0.9.0

- A new `SRec` type for constant time field access for records with densely packed `Storable` fields. Conversion from `Rec` is accomplished with `toSRec`, while `fromSRec` takes you back to `Rec`. Record updates are fairly slow compared to native Haskell records and even `Rec`, but reading a field is as fast as anything.

- Concise record construction syntax from tuples. Construct a `FieldRec` with `fieldRec (#x =: True, #y =: 'b')` and have the type inferred as `Rec ElField '[ '("x", Bool), '("y", Char) ]`. Or use `record` to build records of any functor. Thanks to @heptahedron on GitHub for prompting this feature, and @sboosali for thinking through various approaches.

- Optional concise record field lens syntax. This uses an orphan `IsLabel` instance for all function types, so will conflict with any other library that does the same. Thus it is entirely opt-in: to enable this syntax, you must explicitly `import Data.Vinyl.Syntax`. This enables the use of labels as lenses. For example, `myRec & #name %~ map toUpper` to apply `map toUpper` to the `#name` field of the record value `myRec`. This technique is thanks to Tikhon Jelvis who shared it on the Haskell-Cafe mailing list.

- Field lenses can now change the type of a record. Thanks to @heptahedron on GitHub for exploring this feature. Using the above-mentioned features, one might now write something like `myRec & #name %~ length` to produce a record whose `#name` field is the length of the`String` `#name` field of some record value, `myRec`.

- Changed the type of `=:=` again to work directly with `Label`s as this is the most convenient usage.

- Definitions in `Data.Vinyl.Core` are now consistently in terms of type classes. This permits inlining and specialization to a user's record types. In the case where the record type is known, call sites do not change. But for functions polymorphic in the record's fields, a constraint will be required. If those constraints are a nuisance, or compile times increase beyond comfort, users should use definitions from the `Data.Vinyl.Recursive` that are written in a recursive style (as in previous versions of the `vinyl` package), treating the record as a list of fields.

- Added `restrictCoRec` and `weakenCoRec` suggested by @ElvishJerricco

# 0.8.0

- Overhaul of `FieldRec`: records with named fields. We now take advantage of the `-XOverloadedLabels` extension to support referring to record fields by names such a `#myField`.

- A new `ARec` type for constant-time field access. You can convert a classic, HList-like `Rec` into an `ARec` with `toARec`, or back the other way with `fromARec`. An `ARec` uses an `Array` to store record fields, so the usual trade-offs between lists and arrays apply: lists are cheap to construct by adding an element to the head, but slow to access; it is expensive to modify the shape of an array, but element lookup is constant-time.

**Compatibility Break**: The operator `=:` for constructing a record with a single field has changed. That operation is now known as `=:=`, while `=:` is now used to construct an `ElField`. It was decided that single-field record construction was not a common use-case, so the shorter name could be used for the more common operation. Apologies for making the upgrade a bit bumpy.

# 0.7.0
- Simplified `match`
- Added `Data.Vinyl.Curry`

# 0.6.0

Added a `CoRec` (co-record) type constructed in the same style as the existing `Rec` type for records. A `CoRec` is an open sum type: a value of `CoRec [a,b,c]` is either an `a`, a `b`, *or* a `c`. In contrast a `Rec [a,b,c]` includes an `a`, a `b`, *and*, a `c`.

# 0.5.3

Added a concise `Show` instance for `Const`.

# 0.5.2

Ported the tutorial to haddocks (andrewthad)

# 0.5.1

Added utilities for working with the `FieldRec` type.

# 0.5

Vinyl 0.5 combines the generality of Vinyl 0.4 with the ease-of-use of previous
versions by eschewing the defunctionalized type families and just using plain
type constructors; Vinyl 0.4-style records can be recovered in most cases in a
modular manner without baking it into the fabric of Vinyl itself.

Also new in 0.5 is a unified lens-based approach to subtyping, coercion and
projection.

# 0.4

Vinyl 0.4 is a big departure from previous versions, in that it introduces a
universe encoding as a means to generalize the space of keys from strings to
any arbitrary space. This means that you can have closed universes for your
records.

For details on how to use the new Vinyl, please see `tests/Intro.lhs` or view
Jon's talk at BayHac 2014, [Programming in
Vinyl](http://www.jonmsterling.com/posts/2014-05-19-programming-in-vinyl-bayhac.html).
