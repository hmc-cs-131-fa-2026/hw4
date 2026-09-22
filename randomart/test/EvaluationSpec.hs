module EvaluationSpec where

{- Code under test -}
import RandomArtAST
import RandomArtEvaluation

{- Testing libraries -}        
import SpecLib
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property as P

{- Testing restrictions on expression evaluation -}

rangeSpec :: Spec
rangeSpec =  
    describe "Checking the range of expressions..." $  do
        
        -- evaluating any expression gives back a value in the range [-1, 1]
        context "Given inputs for x and y in the range [-1, 1]" $
            it "the result of any provided expression should also be in the range [-1, 1]"$ property $
                \e -> forAll point $ \p -> 
                    let value = (eval e p)
                    in (-1 <= value) && (value <= 1)

{- Evaluation -}
evalSpec :: Spec
evalSpec = 

    describe "Evaluating an expression" $  do
        
        -- eval X (x, _) == x
        context "evaluating x" $
            it "should always give back x" $ property $
                forAll point $ \p -> eval X p `shouldBe` fst p

        -- eval Y (_, y) == y
        context "evaluating y" $
            it "should always give back y" $ property $
                forAll point $ \p -> eval Y p `shouldBe` snd p

        -- evaluating Times
        context "evaluating Times (Times X Y) Y at (0.5, -1)" $
            it "should result in 0.5" $ 
                eval (Times (Times X Y) Y) (0.5, -1) `shouldBe` 0.5

        context "evaluating Times (Times X Y) Y at (-1, 0.5)" $
            it "should result in -0.25" $ 
                eval (Times (Times X Y) Y) (-1, 0.5) `shouldBe` -0.25

        -- evaluating Avg
        context "evaluating Avg (Times X Y) Y at (0.5, -1)" $
            it "should result in -0.75" $ 
                eval (Avg (Times X Y) Y) (0.5, -1) `shouldBe` -0.75

        context "evaluating Avg (Times X Y) Y at (-1, 0.5)" $
            it "should result in 0" $ 
                eval (Avg (Times X Y) Y) (-1, 0.5) `shouldBe` 0

        -- evaluating Sin
        context "evaluating SinPi X at (-0.5, 1)" $
            it "should result in -1" $ 
                eval (SinPi X) (-0.5, 1) `shouldBe` -1

        context "evaluating SinPi Y at (-0.5, 0.5)" $
            it "should result in 1" $ 
                eval (SinPi Y) (-0.5, 0.5) `shouldBe` 1

        -- evaluating Cos
        context "evaluating CosPi X at (0, 1)" $
            it "should result in 1" $ 
                eval (CosPi X) (0, 1) `shouldBe` 1

        context "evaluating CosPi Y at (0, 1)" $
            it "should result in -1" $ 
                eval (CosPi Y) (0, 1) `shouldBe` -1


{- Combine all the specs into a single spec -}
spec :: Spec                
spec =  do describe "Evaluation" $ do
            evalSpec
            rangeSpec


{- Run all the specs -}
main :: IO ()
main = hspec spec