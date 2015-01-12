0.5.1
---------

Added utilities for working with the `FieldRec` type.

Vinyl 0.5
=========

Vinyl 0.5 combines the generality of Vinyl 0.4 with the ease-of-use of previous
versions by eschewing the defunctionalized type families and just using plain
type constructors; Vinyl 0.4-style records can be recovered in most cases in a
modular manner without baking it into the fabric of Vinyl itself.

Also new in 0.5 is a unified lens-based approach to subtyping, coercion and
projection.

Vinyl 0.4
=========

Vinyl 0.4 is a big departure from previous versions, in that it introduces a
universe encoding as a means to generalize the space of keys from strings to
any arbitrary space. This means that you can have closed universes for your
records.

For details on how to use the new Vinyl, please see `tests/Intro.lhs` or view
Jon's talk at BayHac 2014, [Programming in
Vinyl](http://www.jonmsterling.com/posts/2014-05-19-programming-in-vinyl-bayhac.html).
