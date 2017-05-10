import Test.Hspec
import Test.QuickCheck
import Euler202

main :: IO ()
main = hspec $ do
    describe "bounces" $ do
        it "satisfies sample" $ do
            bounces 11 `shouldBe` 2
            bounces 7  `shouldBe` 2
            bounces 17 `shouldBe` 0 -- is leaky
            bounces 1000001 `shouldBe` 80840
    describe "which points are leaky?" $ do
        it "is revealed" $ do
            let leaks = fst $ leakyPoints 1000001 in
                leaks `shouldBe` [1..3]

