import Test.Hspec
import Test.QuickCheck
import Lib

import qualified Data.HashTable.IO as H

main :: IO ()
main = hspec $ do
    -- describe "incre1" $ do
    --     it "Incre every existing element" $ do
    --         do  ht <- H.new
    --             upsert ht (1,1)
    --             upsert ht (2,2)
    --             incre1 ht 2
    --             xss <- H.toList ht
    --             -- xss `shouldBe` [((1,1),1)]
    --             length xss `shouldBe` 4
    --     it "Incre every existing element, again" $ do
    --         do  ht <- H.new
    --             upsert ht (1,1)
    --             upsert ht (2,1)
    --             upsert ht (3,1)
    --             incre1 ht 2
    --             xss <- H.toList ht
    --             -- xss `shouldBe` [((1,1),1)]
    --             length xss `shouldBe` 6
    -- describe "uniqueSums" $ do
    --     it "gets the empty combos for empty set" $ do
    --         do  ht <- H.new
    --             uniqueSums [] ht
    --             xss <- H.toList ht
    --             length xss `shouldBe` 0
    --     it "gets 1 for 1" $ do
    --         do  ht <- H.new
    --             uniqueSums [1] ht
    --             xss <- H.toList ht
    --             length xss `shouldBe` 1
    --     it "gets 3 for 2" $ do
    --         do  ht <- H.new
    --             uniqueSums [1, 2] ht
    --             xss <- H.toList ht
    --             length xss `shouldBe` 3
        -- it "correct combinations" $ do
        --     do  ht <- H.fromList [((8,3),1),((3,2),1),((5,2),2),((3,1),1),((4,1),1),((10,4),1),((1,1),1),((6,3),1),((4,2),1),((6,2),1),((2,1),1),((7,2),1),((7,3),1),((9,3),1)]
        --         uniqueSums [5] ht
        --         xss <- H.toList ht
        --         countOcc xss `shouldBe` [((1,1),1)]
        --         countOcc xss 5 `shouldBe` 1
        --         countOcc xss 4 `shouldBe` 5
        --         -- filterByLen 3 xss `shouldBe` [((1,1), 1)]
        --         countOcc xss 3 `shouldBe` 10
        --         countOcc xss 2 `shouldBe` 10
        --         countOcc xss 1 `shouldBe` 5
        it "correct combinations with 3 out 6" $ do
            do  ht <- H.new
                uniqueSums [1,3,6,8,10,11] ht
                xss <- H.toList ht
                countOcc xss 3 `shouldBe` 20
        it "is correct for the sample" $ do
            do  ht <- H.new
                uniqueSums [1,3,6,8,10,11] ht
                xss <- H.toList ht
                let unik = filterUnique xss 3
                    in
                        length unik `shouldBe` 8
        it "is correct for the sample 2" $ do
            do  ht <- H.new
                uniqueSums [10,12,14,18,21,25,27,29] ht
                xss <- H.toList ht
                let unik = filterUnique xss 3
                    in
                        length unik `shouldBe` 156

filterByLen :: Int -> [((Int, Int), Int)] -> [((Int, Int), Int)]
filterByLen l = filter (\((sum, len), occ) -> len == l)

countOcc :: [((Int, Int), Int)] -> Int -> Int
countOcc xs len =
    sum mapLen
    where mapLen = map (\((_, l), occ) -> if l == len then occ else 0) xs
