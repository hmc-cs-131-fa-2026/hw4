module ExampleRegex where

import RegexAST
import RegexEvaluation

--
-- Some simple test cases
--

test1 :: Matches
test1 = rexpMatches (Star (Letter 'a')) "aaab"

test1a :: Maybe Match
test1a = rexpMatch (Star (Letter 'a')) "aaab"


test2 :: Matches
test2 = rexpMatches (Star (Alt (Letter 'a') (Letter 'b'))) "abbaca"

test2a :: Maybe Match

test2a = rexpMatch (Star (Alt (Letter 'a') (Letter 'b'))) "abbaca"

test3 :: Matches
test3 = rexpMatches (Concat (Star (Letter 'a')) (Letter 'b')) "aaab"

test3a :: Maybe Match
test3a = rexpMatch (Concat (Star (Letter 'a')) (Letter 'b')) "aaab"

-- these two should fail with no matches
--
test4 :: Matches
test4 = rexpMatches (Concat (Star (Letter 'a')) (Letter 'b')) "aaa"

test4a :: Maybe Match
test4a = rexpMatch (Concat (Star (Letter 'a')) (Letter 'b')) "aaa"
