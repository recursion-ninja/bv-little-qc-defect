## Efficient little-endian bit-vector Haskell library

This package contains a efficient implementation of little-endian bit-vectors. It implements most applicable type-classes and also conversions to and from signed or unsigned numbers. Care has been taken to balance the number of transitive dependencies with respect to functionality provided.

If you need big-endian bit-vectors, use the [`bv`](https://hackage.haskell.org/package/bv) package instead of this one.

#### Tests

The test-suite ensures that all type-class instances are "lawful" and that data-structure specific functionality is well defined.

The `TestSuite.hs` file contains the specification. It can be run by invoking either of the following commands:

  * `cabal test`

  * `stack test`

#### Benchmarks

The benchmarks provide an empirical check for the asymptotic complexity of data-structure operations and also provides an easy metric for detecting performance regressions.

The `Benchmaks.hs` file contains these metrics. It can be run by invoking either of the following commands:

  * `cabal bench`

  * `stack bench`

