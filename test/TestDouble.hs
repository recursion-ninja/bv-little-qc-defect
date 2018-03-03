module Main ( main ) where

import Data.ExampleType
import Test.QuickCheck


main :: IO ()
main = putStrLn "d ∷ Double  ⇒  problemFunction d ≠ ⊥"
     >> quickCheck workingDouble


workingDouble :: Double -> Bool
workingDouble i = problemFunction (f i) `seq` True
  where
    f = floor . abs
