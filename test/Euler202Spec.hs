import Test.Hspec
import Test.QuickCheck
import Euler202

main :: IO ()
main = hspec $ do
    describe "bounces" $ do
        it "satisfies sample" $ do
            bounces 11 `shouldBe` 2
            bounces 7  `shouldBe` 2
            bounces 17 `shouldBe` 4

