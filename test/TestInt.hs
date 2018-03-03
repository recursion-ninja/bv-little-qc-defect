module Main ( main ) where

import Data.ExampleType
import Test.QuickCheck


main :: IO ()
main = putStrLn "i ∷ Int     ⇒  problemFunction i ≠ ⊥"
     >> quickCheck workingInt


workingInt :: Int -> Bool
workingInt i = problemFunction (f i) `seq` True
  where
    f = toEnum . abs
