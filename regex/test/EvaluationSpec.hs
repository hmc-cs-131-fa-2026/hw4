module EvaluationSpec where

{- Code under test -}
import RegexAST
import RegexEvaluation

{- Testing libraries -}        
import Test.Hspec
import Test.QuickCheck
import Data.Set (Set)
import qualified Data.Set as Set

{- Evaluation -}
evalSpec :: Spec
evalSpec = 

    describe "Evaluating a regular expression" $  do

        -- epsilon matches every string s
        -- with results [("", s)]
        context "matching regexp `epsilon` against any string s" $
            it "should be [(\"\", s)]" $ property $
                \s -> rexpMatches Epsilon s `shouldBe` [("", s)]
                
        -- a specific character c matches against the string cs
        -- with results [([c], s)]
        context "matching regexp c against any string \"cs\"" $
            it "should be [([c], s)]" $ property $
                \c s -> rexpMatches (Letter c) (c:s) `shouldBe` [([c], s)]

        -- dot matches against the string cs
        -- with results [([c], s)]
        context "matching regexp . against any string \"cs\"" $
            it "should be [([c], s)]" $ property $
                \c s -> rexpMatches Dot (c:s) `shouldBe` [([c], s)]

        -- dot does not matches the empty string
        context "matching regexp . against the empty string" $
            it "should not find a match" $ 
                rexpMatch Dot "" `shouldBe` Nothing

        -- a matches "aaab"
        -- with results [("a", "aab")]
        context "matching regexp a against aaab" $
            it "should be [(\"a\", \"aab\")]" $ 
                rexpMatches (Letter 'a') "aaab" `shouldBe` [("a", "aab")]

        -- a does not match "baaa"
        context "matching regexp a against baaa" $
            it "should not find a match" $ 
                rexpMatch (Letter 'a') "baaa" `shouldBe` Nothing

        -- a | b matches "ab"
        -- with results [("a", "b")]
        context "matching regexp a|b against ab" $
            it "should be [(\"a\", \"b\")]" $ 
                rexpMatches (Alt (Letter 'a') (Letter 'b')) "ab" `shouldBe` [("a", "b")]

        -- a | b matches "ba"
        -- with results [("b", "a")]
        context "matching regexp a|b against ba" $
            it "should be [(\"b\", \"a\")]" $ 
                rexpMatches (Alt (Letter 'a') (Letter 'b')) "ba" `shouldBe` [("b", "a")]

        -- a | b does not match "cab"
        context "matching regexp a|b against cab" $
            it "should not find a match" $ 
                rexpMatch (Alt (Letter 'a') (Letter 'b')) "cab" `shouldBe` Nothing

        -- ab matches "ab"
        -- with results [("ab", "")]
        context "matching regexp ab against ab" $
            it "should be [(\"ab\", \"\")]" $ 
                rexpMatches (Concat (Letter 'a') (Letter 'b')) "ab" `shouldBe` [("ab", "")]

        -- a* matches "aaab" with results 
        --   [("", "aaab"), ("a", "aab"), ("aa", "ab"), ("aaa", "b")]
        context "matching regexp a* against aaab" $
            it "should be {(\"\", \"aaab\"), (\"a\", \"aab\"), (\"aa\", \"ab\"), (\"aaa\", \"b\")}" $ 
                Set.fromList (rexpMatches (Star (Letter 'a')) "aaab") `shouldBe` Set.fromList [("", "aaab"), ("a", "aab"), ("aa", "ab"), ("aaa", "b")]

        -- (a|b)* matches "abbaca" with 5 results 
        context "matching regexp (a|b)* against abbaca" $
            it "should find 5 results" $ 
                length (rexpMatches (Star (Alt (Letter 'a') (Letter 'b'))) "abbaca") `shouldBe` 5

        -- (a*)b matches "aaab" with 
        -- results [("aaab"), ""]
        context "matching regexp (a*)b against aaab" $
            it "should be [(\"aaab\"), \"\"]" $ 
                rexpMatches (Concat (Star (Letter 'a')) (Letter 'b')) "aaab" `shouldBe` [("aaab", "")]

        -- (a*)b should not match "aaa"
        context "matching regexp (a*)b against aaa" $
            it "should not find a match" $ 
                rexpMatch (Concat (Star (Letter 'a')) (Letter 'b')) "aaa" `shouldBe` Nothing


{- Combine all the specs into a single spec -}
spec :: Spec                
spec =  do describe "Evaluation" $ do
            evalSpec


{- Run all the specs -}
main :: IO ()
main = hspec spec