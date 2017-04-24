import Test.Hspec
import Test.QuickCheck
import Euler200

main :: IO ()
main = hspec $ do
    describe "squbes" $ do
        it "is ordered ascending" $ do
            isAscending (take 10 squbes) `shouldBe` True

isAscending :: Ord a => [a] -> Bool
isAscending [] = True
isAscending (x:[]) = True
isAscending (x:y:xs) = y >= x && isAscending (y:xs)