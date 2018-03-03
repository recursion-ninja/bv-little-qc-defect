{-# LANGUAGE FlexibleInstances #-}

-- We apply this to suppress the deprecated warning cause by calls to 'bitSize'
-- If there is a more fine-grained way to supress this warning without suppressing
-- deprecated warnings for the whole module, we should do that instead.
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Main ( main ) where

import Data.ExampleType
import Test.Tasty
import Test.Tasty.QuickCheck hiding ((.&.))


main :: IO ()
main = defaultMain testSuite


testSuite :: TestTree
testSuite = testGroup "BitVector tests"
    [ testProperty "When i :: Int,  0 < i ==> (problemFunction i bv) =/= _|_" workingInt
    , testProperty "When w :: Word, 0 < w ==> (problemFunction w bv) =/= _|_" brokenWord
    ]
  where
    workingInt :: Int -> ExampleType -> Property
    workingInt i bv =
        True ==> problemFunction (f i) bv `seq` () === ()
      where
        f = toEnum . abs

    brokenWord :: Word -> ExampleType -> Property
    brokenWord w bv =
        True ==> problemFunction w bv `seq` () === ()
