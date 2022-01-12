module SubmarineSpec (spec) where

import Test.Hspec
import Submarine


spec :: Spec
spec = do
    describe "followV1" $ do
        describe "'forward X'" $ do
            it "increases the horizontal position by X units" $ do
                let commands = [("forward", 1), ("forward", 2)]
                let (position, _depth, _aim) = followV1 commands
                position `shouldBe` 3

        describe "'down X'" $ do
            it "increases the depth by X units" $ do
                let commands = [("down", 1), ("down", 2)]
                let (_position, depth, _aim) = followV1 commands
                depth `shouldBe` 3

        describe "'up X'" $ do
            it "decreases the depth by X units" $ do
                let commands = [("up", 1), ("up", 2)]
                let (_position, depth, _aim) = followV1 commands
                depth `shouldBe` -3

        it "returns a tuple with the final depth, position, and aim" $ do
            let commands = [("down", 10), ("forward", 3), ("up", 3), ("forward", 5)]
            followV1 commands `shouldBe` (8, 7, 0)

    describe "followV2" $ do
        describe "'forward X'" $ do
            it "increases the horizontal position by X units" $ do
                let commands = [("forward", 1), ("forward", 2)]
                let (position, _depth, _aim) = followV2 commands
                position `shouldBe` 3

            it "increases the depth by the aim multiplied by X" $ do
                let commands = [("down", 3), ("forward", 2)]
                let (_position, depth, _aim) = followV2 commands
                depth `shouldBe` 6

        describe "'down X'" $ do
            it "increases the aim by X units" $ do
                let commands = [("down", 1), ("down", 2)]
                let (_position, _depth, aim) = followV2 commands
                aim `shouldBe` 3

        describe "'up X'" $ do
            it "decreases the aim by X units" $ do
                let commands = [("up", 1), ("up", 2)]
                let (_position, _depth, aim) = followV2 commands
                aim `shouldBe` -3

        it "returns a tuple with the final depth, position, and aim" $ do
            let commands = [("down", 10), ("forward", 3), ("up", 3), ("forward", 5)]
            followV2 commands `shouldBe` (8, 65, 7)
