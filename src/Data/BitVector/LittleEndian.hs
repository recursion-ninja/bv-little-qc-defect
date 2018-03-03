{-# LANGUAGE BangPatterns #-}

module Data.BitVector.LittleEndian
  ( BitVector()
  , subRange
  ) where


import Test.QuickCheck (Arbitrary(..), NonNegative(..), suchThat)


-- |
-- A little-endian bit vector of non-negative dimension.
data  BitVector
    = BV
    { dim :: !Word
    , nat :: !Integer
    }


-- |
-- /Since: 0.1.0.0/
instance Arbitrary BitVector where

    arbitrary = do
        dimVal <- toEnum . getNonNegative <$> arbitrary
        let upperBound = 2^dimVal
        intVal <- (getNonNegative <$> arbitrary) `suchThat` (< upperBound)
        pure $ BV dimVal intVal


-- |
-- /Since: 0.1.0.0/
instance Show BitVector where

    show (BV w n) = mconcat [ "[", show w, "]", show n ]


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
  | otherwise     = BV m $ 2^m
  where
    m = upper - lower + 1


