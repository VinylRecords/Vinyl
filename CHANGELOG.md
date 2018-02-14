# 0.9.0
- A new `SRec` type for constant time field access for records with densely packed `Storable` fields. Conversion from `Rec` is accomplished with `toSRec`, while `fromSRec` takes you back to `Rec`. Record updates are fairly slow compared to native Haskell records and even `Rec`, but reading a field is as fast as anything.

- Changed the type of `=:=` again to work directly with `Label`s as this is the most convenient usage.

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
