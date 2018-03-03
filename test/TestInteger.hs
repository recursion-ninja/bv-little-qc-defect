module Main ( main ) where

import Data.ExampleType
import Test.QuickCheck


main :: IO ()
main = putStrLn "n ∷ Integer ⇒  problemFunction n ≠ ⊥"
     >> quickCheck workingInteger


workingInteger :: Integer -> Bool
workingInteger i = problemFunction (f i) `seq` True
  where
    f = fromInteger
