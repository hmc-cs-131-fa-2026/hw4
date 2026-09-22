{-|
Module       : RandomArtAST
Description  : Defines abstract syntax for expressions over two
               floating-point numbers.
Maintainer   : CS 131, Programming Languages (Melissa O'Neill, Chris Stone, Ben Wiedermann)
-}

module RandomArtAST where

-- | Representation of expressions over two floating-point numbers, X and Y
--
--   All operators **must** return values in the interval [-1.0, 1.0],
--   when their arguments are in this same range. Thus, product and average are ok.
--   Addition is not ok, because the result could be < -1.0 or > 1.0
data Exp    = X                   -- ^ x's value
            | Y                   -- ^ y's value 
            | Times Exp Exp       -- ^ product of e1 and e2
            | Avg   Exp Exp       -- ^ average of e1 and e2
            | SinPi Exp           -- ^ sin (pi * e)
            | CosPi Exp           -- ^ cos (pi * e)
    deriving (Show, Read, Eq, Ord)

