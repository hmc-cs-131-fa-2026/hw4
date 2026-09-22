{-|
Module       : RegexAST
Description  : Defines the abstract syntax for regular expressions
Maintainer   : CS 131, Programming Languages (Melissa O'Neill, Chris Stone, Ben Wiedermann)
-}

module RegexAST where

-- | A regular expression (abbreviated as regexp)
data RegExp = Epsilon                   -- ^ Nothing (the empty string)
            | Letter Char               -- ^ A single, specific character
            | Dot                       -- ^ Any character
            | Alt    RegExp RegExp      -- ^ Either first OR second regexp
            | Concat RegExp RegExp      -- ^ First regexp FOLLOWED BY second regexp
            | Star   RegExp             -- ^ Zero or more REPETITIONS of a regexp
