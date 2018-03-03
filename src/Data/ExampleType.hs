module Data.ExampleType
  ( ExampleType()
  , fineFunction
  , problemFunction
  ) where


-- |
-- Example type, the strictness in the second value is important.
-- Without the strictness in the second argumnet, the problem function terminates.
data ExampleType = C Word !Integer deriving (Show)


-- |
-- A minimal working example of the defect involving the Word type.
fineFunction :: Int -> ExampleType
fineFunction w = C (toEnum (abs w)) $ 2^w


-- |
-- A minimal working example of the defect involving the Word type.
problemFunction :: Word -> ExampleType
problemFunction w = C w $ 2^w

