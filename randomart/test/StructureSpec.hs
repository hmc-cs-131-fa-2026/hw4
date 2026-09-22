{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module StructureSpec where

{- Code under test -}
import           RandomArtAST
import           RandomArtEvaluation

{- Testing libraries -}
import           SpecLib
import           Test.Hspec
import           Test.QuickCheck

import qualified Data.List
import qualified Generics.SYB

deriving instance Generics.SYB.Typeable Exp
deriving instance Generics.SYB.Data Exp


{- Structure -}
exprConstructors =
  Generics.SYB.dataTypeConstrs (Generics.SYB.dataTypeOf RandomArtAST.X)

numProvidedConstructors = 6
numNewConstructors = (length exprConstructors) - numProvidedConstructors

structureSpec :: Spec
structureSpec =
    describe "Adding expressiveness to the language" $ do

        context "The number of new constructors" $
            it "should be at least 2" $
                numNewConstructors `shouldSatisfy` (>= 2)


{- Combine all the specs into a single spec -}
spec :: Spec
spec =  do describe "Evaluation" $ do
            structureSpec


{- Run all the specs -}
main :: IO ()
main = hspec spec
