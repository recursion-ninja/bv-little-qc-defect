## Efficient little-endian bit-vector Haskell library

[![Build Status](https://travis-ci.org/recursion-ninja/bv-little.svg?branch=master)](https://travis-ci.org/recursion-ninja/bv-little)
[![Coverage Status](https://coveralls.io/repos/github/recursion-ninja/bv-little/badge.svg?branch=master)](https://coveralls.io/github/recursion-ninja/bv-little?branch=master)
[![License FreeBSD](https://img.shields.io/badge/license-FreeBSD-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/bv-little.svg?style=flat)](https://hackage.haskell.org/package/bv-little)
[![Stackage Nightly](http://stackage.org/package/bv-little/badge/nightly)](http://stackage.org/nightly/package/bv-little)
[![Stackage LTS](http://stackage.org/package/bv-little/badge/lts)](http://stackage.org/lts/package/bv-little)


This package contains an efficient implementation of little-endian bit-vectors. It implements most applicable type-classes and also conversions to and from signed or unsigned numbers. Care has been taken to balance the number of transitive dependencies with respect to functionality provided.

For an implementation of bit vectors which are isomorphic to a `Bool` with the /most/ significant bit at the head of the list and the /least/ significant bit at the end of the list, use the [`bv`](https://hackage.haskell.org/package/bv) package.

#### Tests

The test suite ensures that all type class instances are "lawful" and that data-structure&#8211;specific functionality is well defined.

The `TestSuite.hs` file contains the specification. It can be run by invoking any of the following commands:

  * `cabal new-test`

  * `cabal test`

  * `stack test`

#### Benchmarks

The benchmarks provide an empirical check for the asymptotic complexity of data structure operations and also provide easy metrics for detecting performance regressions.

The `Benchmaks.hs` file contains these metrics. It can be run by invoking any of the following commands:

  * `cabal new-bench`

  * `cabal bench`

  * `stack bench`
