module Chapters.WordNumberSpec (spec) where

import           Chapters.WordNumber (digitToWord, digits, wordNumber)
import           Test.Hspec

spec :: Spec
spec = do
    describe "digitToWord" $ do
        it "returns zero for 0" $
            digitToWord 0 `shouldBe` "zero"
        it "returns one for 1" $
            digitToWord 1 `shouldBe` "one"
    describe "digits" $ do
        it "returns [1] for 1" $
            digits 1 `shouldBe` [1]
        it "returns [1, 0, 0] for 100" $
            digits 100 `shouldBe` [1,0,0]
    describe "wordNumber" $ do
        it "one-zero-zero given 100" $
            wordNumber 100 `shouldBe` "one-zero-zero"
        it "nine-zero-zero-one for 9001" $
            wordNumber 9001 `shouldBe` "nine-zero-zero-one"
