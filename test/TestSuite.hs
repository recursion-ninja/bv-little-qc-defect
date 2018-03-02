{-# LANGUAGE FlexibleInstances #-}

-- We apply this to suppress the deprecated warning cause by calls to 'bitSize'
-- If there is a more fine-grained way to supress this warning without suppressing
-- deprecated warnings for the whole module, we should do that instead.
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Main ( main ) where

import Data.Bits
import Data.BitVector.LittleEndian
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Hashable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid ()
import Data.MonoTraversable
import Data.Semigroup
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck hiding ((.&.))


main :: IO ()
main = defaultMain testSuite


testSuite :: TestTree
testSuite = testGroup "BitVector tests"
    [ bitVectorProperties
    ]


bitVectorProperties :: TestTree
bitVectorProperties = testGroup "BitVector properties"
    [ testProperty "i <= j ==> dimension . subRange (i,j) === const (j - i)" subRangeFixedDimension
    ]
  where
{-
    subRangeFixedDimension :: (Int, Int) -> BitVector -> Property
    subRangeFixedDimension (lower, upper) bv =
        f lower <= f upper ==> subRange (f lower, f upper) bv `seq` () === ()
      where
        f = toEnum . abs
-}
    subRangeFixedDimension :: (Word, Word) -> BitVector -> Property
    subRangeFixedDimension (lower, upper) bv =
        lower <= upper ==> subRange (lower, upper) bv `seq` () === ()
