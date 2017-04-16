import Test.Hspec
import Test.QuickCheck
import Lib

import qualified Data.HashTable.IO as H

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
                incre1 ht (2)
                xss <- H.toList ht
                xss `shouldBe` [((1,1),1)]
