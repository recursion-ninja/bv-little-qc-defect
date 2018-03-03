module Main ( main ) where

import Data.ExampleType
import Test.QuickCheck


main :: IO ()
main = putStrLn "w ∷ Word    ⇒  problemFunction w = ⊥"
     >> quickCheck brokenWord


brokenWord :: Word -> Bool
brokenWord w = problemFunction w `seq` True
