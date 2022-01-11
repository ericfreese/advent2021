module SonarSpec (spec) where

import Test.Hspec
import Sonar


spec :: Spec
spec = do
    describe "countIncreases" $ do
        it "counts the number of times that numbers in a list increase" $ do
            countIncreases [5, 5, 6, 1, 8] `shouldBe` 2

    describe "slidingWindows" $ do
        it "returns a list with all windows of the specified length" $ do
            slidingWindows 2 [1..4] `shouldBe` [[1, 2], [2, 3], [3, 4]]
            slidingWindows 3 [1..4] `shouldBe` [[1, 2, 3], [2, 3, 4]]
