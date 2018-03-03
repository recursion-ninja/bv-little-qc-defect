module Main ( main ) where

import Data.ExampleType
import Test.QuickCheck


main :: IO ()
main = putStrLn "w ∷ Word    ⇒  problemFunction w = ⊥"
     >> quickCheck brokenWord


brokenWord :: Word -> Bool
brokenWord w = problemFunction (f w) `seq` True
  where
    f = toEnum . fromEnum
