module Lib where

import qualified Data.HashTable.IO as H

type SumState = ((Int, Int), Int)
type SumTable = H.BasicHashTable (Int, Int) Int

upsert :: SumTable -> (Int, Int) -> IO ()
upsert ht k =
        do  maybeV <- H.lookup ht k
            putStr $ "looking for " ++ (show k) ++ "found" ++ (show maybeV) ++ "\r\n"
            case    maybeV of
                    Just v ->
                        do  putStr $ "updating" ++ (show k) ++ "with" ++ (show (v + 1))  ++ "\r\n"
                            H.insert ht k (v + 1)
                    Nothing -> 
                        do  putStr $ "inserting" ++ (show k) ++ "with 1" ++ "\r\n"
                            H.insert ht k 1

incre1 :: SumTable -> Int -> IO ()
incre1 ht n = 
    do  fstates <- H.foldM folder iniState ht
        fstates ()
        where 
            iniState :: () -> IO ()
            iniState () = return ()
            increByKey :: SumState -> IO ()
            increByKey ((sum, len), v) = 
                upsert ht (sum + n, len + 1)
            folder :: (() -> IO ()) -> SumState -> IO (() -> IO ())
            folder k sm =
                let res = \k' -> do k ()
                                    increByKey sm
                    in return res

uniqueSums :: [Int] -> SumTable -> IO ()
uniqueSums [] ht = return ()
uniqueSums (x:xs) ht = do incre1 ht x; upsert ht (x, 1); uniqueSums xs ht

filterUnique :: [((Int, Int), Int)] -> Int -> [((Int, Int), Int)]
filterUnique xs l = 
    do  tup@((sum, len), dup) <- xs
        do  True <- return (len == l && dup == 1)
            return tup

main_ :: IO ()
main_ = 
    let ht' :: IO(SumTable)
        ht' = H.new in
        do  ht <- ht'
            uniqueSums [1,3,6,8,10,11] ht
            do  xss <- H.toList ht
                let qualified = filterUnique xss 3 in
                    putStr $ show qualified

