module SpecLib
    ( module Test.Hspec
    , module Test.QuickCheck
    , module RandomArtAST
    , value
    , point
    -- , expr
    ) where

{- Code under test -}
import RandomArtAST
import RandomArtEvaluation


{- Testing libraries -}        
import Test.Hspec
import Test.QuickCheck

{- Generator for x & y values -}
value :: Gen Float
value = choose(-1, 1)

point :: Gen Point
point = do x <- value
           y <- value
           return (x, y)

{- Generator for Exprs -}
instance Arbitrary Exp where
    arbitrary = sized expr
    
expr 0 = do 
    oneof [return X, return Y]

expr n | n > 0 = do
    subexpr <- expr (n-1)
    oneof [  return X
           , return Y
           , return (Times subexpr subexpr)
           , return (Avg subexpr subexpr)
           , return (SinPi subexpr)
           , return (CosPi subexpr) ]
