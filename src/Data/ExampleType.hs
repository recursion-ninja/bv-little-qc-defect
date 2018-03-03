{-# LANGUAGE BangPatterns #-}

module Data.ExampleType
  ( ExampleType()
  , problemFunction
  ) where


import Test.QuickCheck (Arbitrary(..), NonNegative(..), suchThat)


-- |
-- A little-endian bit vector of non-negative dimension.
data  ExampleType = C !Word !Integer


-- |
-- Should be a simple instance.
instance Arbitrary ExampleType where

    arbitrary = do
        dimVal <- toEnum . getNonNegative <$> arbitrary
        let upperBound = 2^dimVal
        intVal <- (getNonNegative <$> arbitrary) `suchThat` (< upperBound)
        pure $ C dimVal intVal


instance Show ExampleType where

    show (C w n) = mconcat [ "[", show w, "]", show n ]


-- |
-- A minimal working example of the defect involving the Word type.
{-# INLINE problemFunction #-}
problemFunction :: Word -> ExampleType -> ExampleType
problemFunction (!lower) (C _ n) = C m $ 2^m
  where
    m = lower + 1


