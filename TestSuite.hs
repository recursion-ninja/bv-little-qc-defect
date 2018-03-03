module Main ( main ) where

import Data.ExampleType
import Test.Tasty
import Test.Tasty.QuickCheck


main :: IO ()
main = defaultMain testSuite


testSuite :: TestTree
testSuite = testGroup "Example tests"
    [ testProperty "d ∷ Double  ⇒  problemFunction d ≠ ⊥" workingDouble
    , testProperty "n ∷ Integer ⇒  problemFunction n ≠ ⊥" workingInt
    , testProperty "i ∷ Int     ⇒  problemFunction i ≠ ⊥" workingInt
    , testProperty "w ∷ Word    ⇒  problemFunction w = ⊥" brokenWord
    ]
  where
    workingDouble :: Double -> Bool
    workingDouble i = problemFunction (f i) `seq` True
      where
        f = floor . abs

    workingInteger :: Integer -> Bool
    workingInteger i = problemFunction (f i) `seq` True
      where
        f = fromInteger

    workingInt :: Int -> Bool
    workingInt i = problemFunction (f i) `seq` True
      where
        f = toEnum . abs

    brokenWord :: Word -> Bool
    brokenWord w = problemFunction (f w) `seq` True 
      where
        f = toEnum . fromEnum
