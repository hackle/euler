module Main where

import qualified Data.HashTable.IO as H

type SumTable = H.BasicHashTable (Int, Int) Int

upsert :: SumTable -> (Int, Int) -> IO ()
upsert ht k =
        do  maybeV <- H.lookup ht k
            case    maybeV of
                    Just v -> H.insert ht k (v + 1)
                    Nothing -> H.insert ht k 1

-- uniqueSums :: [Int] -> IO (HashTable Int Int)
-- uniqueSums [] ht = ht
-- uniqueSums (x:[]) ht' = 
--     do  ht <- ht'
--         H.insert ht (x 1, 1); ht

-- foo = do
--     ht <- H.new
--     H.insert ht 1 1
--     H.insert ht 2 4 
--     H.insert ht 3 9
--     return ht

main :: IO ()
main = 
    let ht' :: IO(SumTable)
        ht' = H.new in
        do  ht <- ht'
            upsert ht (1, 1)
            upsert ht (1, 1)
            upsert ht (1, 1)
            upsert ht (1, 1)
            upsert ht (1, 2)
            do  xss <- H.toList ht
                putStr $ show xss