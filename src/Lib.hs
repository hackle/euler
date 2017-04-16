module Lib where

import qualified Data.HashTable.IO as H

type SumState = ((Int, Int), Int)
type SumTable = H.BasicHashTable (Int, Int) Int

upsert :: SumTable -> (Int, Int) -> IO ()
upsert ht k =
        do  maybeV <- H.lookup ht k
            case    maybeV of
                    Just v -> H.insert ht k (v + 1)
                    Nothing -> H.insert ht k 1

type FoldState = SumState -> IO ()

incre1 :: SumTable -> Int -> IO ()
incre1 ht n = 
    do  fstates <- H.foldM folder iniState ht
        do fstates
        where 
            iniState :: IO ()
            iniState = return ()
            increByKey :: SumState -> IO ()
            increByKey ((sum, len), v) = 
                upsert ht (sum + n, len + 1)
            folder :: IO () -> SumState -> IO (IO ())
            folder fs sm =
                do  fs
                    return (increByKey sm)

uniqueSums :: [Int] -> SumTable -> IO ()
uniqueSums [] ht = return ()
uniqueSums (x:xs) ht = do incre1 ht x; upsert ht (x, 1); uniqueSums xs ht

filterUnique :: [((Int, Int), Int)] -> [((Int, Int), Int)]
filterUnique xs = do    tup@((sum, len), dup) <- xs
                        do  True <- return (len == 3 && dup == 1)
                            return tup

main_ :: IO ()
main_ = 
    let ht' :: IO(SumTable)
        ht' = H.new in
        do  ht <- ht'
            uniqueSums [1,3,6,8,10,11] ht
            do  xss <- H.toList ht
                let qualified = filterUnique xss in
                    putStr $ show qualified

