module Main ( main ) where

import Data.ExampleType
import Test.Tasty
import Test.Tasty.QuickCheck


main :: IO ()
main = defaultMain testSuite


testSuite :: TestTree
testSuite = testGroup "Example tests"
    [ testProperty "When i :: Int,     fineFunction i =/= _|_" workingInt
    , testProperty "When i :: Int,  problemFunction i =/= _|_" workingInt
    , testProperty "When w :: Word, problemFunction w =/= _|_" brokenWord
    ]
  where
    equivelentInt :: Int -> Property
    equivelentInt i = fineFunction i `seq` () === ()

    workingInt :: Int -> Property
    workingInt i = problemFunction (f i) `seq` () === ()
      where
        f = toEnum . abs

    brokenWord :: Word -> Property
    brokenWord w = problemFunction w `seq` () === ()
