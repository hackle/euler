import Test.Hspec
import Test.QuickCheck
import Euler200
import Data.List

main :: IO ()
main = hspec $ do
    describe "isPrimeProof200" $ do
        it "judges corrects" $ do
            isPrimeProof200 200 `shouldBe` True
            isPrimeProof200 1992008 `shouldBe` True

    describe "squbes" $ do
        it "is ordered ascending" $ do
            isAscending (take 1000 squbes) `shouldBe` True
    
    describe "Order once" $ do
        it "initializes properly" $ do
            let ori = ([], [], 0, [Products [5..10], Products [11..15]])
                res = ([], [Products [5..10]], 5, [Products [11..15]])
                res1 = ([5], [Products [6, 10], Products [11..15]], 11, [])
                in do
                    orderOnce ori `shouldBe` res
                    (orderOnce $ orderOnce ori) `shouldBe` res1
        it "Shifts head around" $ do
            let ori = ([], [Products [1..4]], 1, [Products [5..10], Products [11..15]])
                res = ([1], [Products [2..4], Products [5..10]], 5, [Products [11..15]])
                in orderOnce ori `shouldBe` res

    describe "Products lists" $ do
        it "is equal by head" $ do
            Products [1..5] `shouldBe` (Products [1..100])

        it "is ordered at insert" $ do
            [Products [1..5], Products [2..5], Products [3..5]] `shouldBe` (insert (Products [3..5]) $ insert (Products [1..5]) [ Products [2..5] ])

        it "is ordered at delete" $ do
            let xs = insert (Products [4..5]) $ insert (Products [1..5]) [ Products [2..5] ]
            [Products [1..5], Products [2..5], Products [3..5]] `shouldBe` (insert (Products [3..5]) $ delete (Products [4..6]) xs)

isAscending :: Ord a => [a] -> Bool
isAscending [] = True
isAscending (x:[]) = True
isAscending (x:y:xs) = if y < x then False else isAscending (y:xs)