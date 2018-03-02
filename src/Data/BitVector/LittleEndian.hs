-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BitVector.LittleEndian
-- Copyright   :  (c) Alex Washburn 2018
-- License     :  BSD-style
--
-- Maintainer  :  github@recursion.ninja
-- Stability   :  provisional
-- Portability :  portable
--
-- A bit vector similar to @Data.BitVector@ from the
-- <https://hackage.haskell.org/package/bv bv>, however the endianness is
-- reversed. This module defines /little-endian/ pseudo–size-polymorphic
-- bit vectors.
--
-- Little-endian bit vectors are isomorphic to a @[Bool]@ with the /least/
-- significant bit at the head of the list and the /most/ significant bit at the
-- end of the list. Consequently, the endianness of a bit vector affects the semantics of the
-- following typeclasses:
--
--   * 'Bits'
--   * 'FiniteBits'
--   * 'Semigroup'
--   * 'Monoid'
--   * 'MonoFoldable'
--   * 'MonoTraversable'
--
-- For an implementation of bit vectors which are isomorphic to a @[Bool]@ with the /most/
-- significant bit at the head of the list and the /least/ significant bit at the
-- end of the list, use the
-- <https://hackage.haskell.org/package/bv bv> package.
--
-- This module does /not/ define numeric instances for 'BitVector'. This is
-- intentional! To interact with a bit vector as an 'Integral' value,
-- convert the 'BitVector' using either 'toSignedNumber' or 'toUnsignedNumber'.
--
-----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, MagicHash #-}
{-# LANGUAGE Trustworthy, TypeFamilies #-}

module Data.BitVector.LittleEndian
  ( BitVector()
  , dimension
  , subRange
  ) where


import Control.DeepSeq
import Data.Bits
import Data.Data
import Data.Foldable
import Data.Hashable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid        ()
import Data.MonoTraversable
import Data.Ord
import Data.Primitive.ByteArray
import Data.Semigroup
import Data.Word
import GHC.Exts
import GHC.Generics
import GHC.Integer.GMP.Internals
import GHC.Integer.Logarithms
import Test.QuickCheck (Arbitrary(..), CoArbitrary(..), NonNegative(..), suchThat)


-- |
-- A little-endian bit vector of non-negative dimension.
data  BitVector
    = BV
    { dim :: {-# UNPACK #-} !Int -- ^ The /dimension/ of a bit vector.
    , nat :: !Integer            -- ^ The value of a bit vector, as a natural number.
    }


-- |
-- /Since: 0.1.0.0/
instance Arbitrary BitVector where

    arbitrary = do
        dimVal <- getNonNegative <$> arbitrary
        let upperBound = 2^dimVal
        intVal <- (getNonNegative <$> arbitrary) `suchThat` (< upperBound)
        pure $ BV dimVal intVal


-- |
-- /Since: 0.1.0.0/
instance Show BitVector where

    show (BV w n) = mconcat [ "[", show w, "]", show n ]


-- |
-- Get the dimension of a 'BitVector'. Preferable to 'finiteBitSize' as it
-- returns a type which cannot represent a non-negative value and a 'BitVector'
-- must have a non-negative dimension.
--
-- /Time:/ \(\, \mathcal{O} \left( 1 \right) \)
--
-- /Since: 0.1.0.0/
--
-- ==== __Examples__
--
-- >>> dimension [2]3
-- 2
--
-- >>> dimension [4]12
-- 4
{-# INLINE dimension #-}
dimension :: BitVector -> Word
dimension = toEnum . dim


-- |
-- Get the /inclusive/ range of bits in 'BitVector' as a new 'BitVector'.
--
-- If either of the bounds of the subrange exceed the bit vector's dimension,
-- the resulting subrange will append an infinite number of zeroes to the end
-- of the bit vector in order to satisfy the subrange request.
--
-- /Time:/ \(\, \mathcal{O} \left( 1 \right) \)
--
-- /Since: 0.1.0.0/
--
-- ==== __Examples__
--
-- >>> subRange (0,2) [4]7
-- [3]7
--
-- >>> subRange (1, 3) [4]7
-- [3]3
--
-- >>> subRange (2, 4) [4]7
-- [3]1
--
-- >>> subRange (3, 5) [4]7
-- [3]0
--
-- >>> subRange (10, 20) [4]7
-- [10]0
{-# INLINE subRange #-}
subRange :: (Word, Word) -> BitVector -> BitVector
subRange (!lower, !upper) (BV _ n)
  | lower > upper = BV 0 0
  | otherwise     = BV m $ (n `shiftR` i) `mod` 2^m
  where
    i = fromEnum lower
    m = fromEnum $ upper - lower + 1
