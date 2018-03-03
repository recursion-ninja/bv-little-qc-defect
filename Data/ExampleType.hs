module Data.ExampleType
  ( ExampleType()
  , problemFunction
  ) where


-- |
-- Example type, the strictness in the second value is important.
-- Without the strictness in the second argumnet, the problem function terminates.
data ExampleType = C !Integer deriving (Show)


-- |
-- A minimal working example of the defect involving the Word type.
problemFunction :: Word -> ExampleType
problemFunction w = C $ 2^w

