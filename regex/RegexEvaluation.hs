{-|
Module       : RegexEvaluation
Description  : How to match a regular-expression pattern against an input string
Maintainer   : CS 131, Programming Languages (Melissa O'Neill, Chris Stone, Ben Wiedermann)
-}

module RegexEvaluation where 

import RegexAST    

--------------------------------------------------------------------------------
-- Type synonyms 
--
-- These types are for the text inputs and outputs to a regular-expression checker.
--------------------------------------------------------------------------------

-- | Type synonym for input text
type Text  = String       

-- | Type synonym for text that matched a regular expression
type Match = String        

-- | Type synonym for all possible regular-expression matches, which includes
--   both the match, and the text left over after the match.
type Matches = [(Match,Text)]
                                                         

--------------------------------------------------------------------------------
-- Regular-expression matching 
--------------------------------------------------------------------------------

-- Useful helper function (hint, hint)
-- But you'll have to read the code carefully to figure out what it does...
prependAllMatches :: Match -> Matches -> Matches
prependAllMatches prefix matches = map prependEachMatch matches
    where prependEachMatch (match, rest) = (prefix ++ match, rest)


-- | Produce a list of ALL POSSIBLE MATCHES of a regular expression, against an
--   input string. A match occurs if the regular-expression pattern matches a 
--   (possibly empty) prefix of the input string. For each match, this function
--   splits the input into the matching prefix and the remainder suffix, then
--   returns a list of all such (match, remainder) pairs.
--   (Remember that Haskell lists are lazy, so matches are created only as needed.)
rexpMatches :: RegExp -> Text -> Matches

-- The empty string matches the empty string and consumes no characters from the input
rexpMatches Epsilon txt = [("", txt)]

-- Repetition
rexpMatches (Star re) txt = 
    rexpMatches (Alt (Concat re (Star re)) Epsilon) txt

-- Any character matches any valid Haskell character and consumes that character
-- from the input.
rexpMatches Dot (c:txt) = [([c], txt)]

-- A single character 
rexpMatches (Letter l) (c:txt) = undefined    -- FIXME

-- Alternatives
rexpMatches (Alt re1 re2) txt = undefined     -- FIXME

-- Concatenation
rexpMatches (Concat re1 re2) txt = undefined  -- FIXME

-- Otherwise, no match
rexpMatches _ _ = []  


-- | Given a regular expression and an input string, return at most one matching
--   prefix of the input string.
rexpMatch :: RegExp -> Text -> Maybe Match
rexpMatch re txt = case rexpMatches re txt of
                       []            -> Nothing
                       (match,_) : _ -> Just match  -- get the "first" matching prefix

