import Test.Hspec
import Test.QuickCheck
import Lib

import qualified Data.HashTable.IO as H

countOcc :: [((Int, Int), Int)] -> Int -> Int
countOcc xs len =
    sum mapLen
    where mapLen = map (\((_, l), occ) -> if l == len then occ else 0) xs

main :: IO ()
main = hspec $ do
    describe "anything" $ do
        it "certainly true" $ do
            1 `shouldBe` 1
    describe "upsert" $ do
        it "inserts if not existent" $ do
            do  ht <- H.new
                upsert ht (1,1)
                maybeV <- H.lookup ht (1,1)
                maybeV `shouldBe` (Just 1)
        it "updates if existent" $ do
            do  ht <- H.new
                upsert ht (1,1)
                upsert ht (1,1)
                maybeV  <- H.lookup ht (1,1)
                maybeV `shouldBe` (Just 2)
    describe "incre1" $ do
        it "Incre every existing element" $ do
            do  ht <- H.new
                upsert ht (1,1)
                upsert ht (2,2)
                incre1 ht 2
                xss <- H.toList ht
                -- xss `shouldBe` [((1,1),1)]
                length xss `shouldBe` 4
        it "Incre every existing element, again" $ do
            do  ht <- H.new
                upsert ht (1,1)
                upsert ht (2,1)
                upsert ht (3,1)
                incre1 ht 2
                xss <- H.toList ht
                -- xss `shouldBe` [((1,1),1)]
                length xss `shouldBe` 6
    describe "uniqueSums" $ do
        it "gets the empty combos for empty set" $ do
            do  ht <- H.new
                uniqueSums [] ht
                xss <- H.toList ht
                length xss `shouldBe` 0
        it "gets 1 for 1" $ do
            do  ht <- H.new
                uniqueSums [1] ht
                xss <- H.toList ht
                length xss `shouldBe` 1
        it "gets 3 for 2" $ do
            do  ht <- H.new
                uniqueSums [1, 2] ht
                xss <- H.toList ht
                length xss `shouldBe` 3
        it "gets 7 for 3" $ do
            do  ht <- H.new
                uniqueSums [1, 2, 3] ht
                xss <- H.toList ht
                countOcc xss 3 `shouldBe` 1
                countOcc xss 2 `shouldBe` 3
                countOcc xss 1 `shouldBe` 3
        it "is correct for the sample" $ do
            do  ht <- H.new
                uniqueSums [1,3,6,8,10,11] ht
                xss <- H.toList ht
                let unik = filter (\((_, len), dup) -> 3 == len) xss
                    in
                -- let unik = filterUnique xss
                        unik `shouldBe` [((1,1), 1)]
