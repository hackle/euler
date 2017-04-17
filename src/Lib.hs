module Lib where

import qualified Data.HashTable.IO as H

type SumState = ((Int, Int), Int)
type SumTable = H.BasicHashTable (Int, Int) Int

incre1 :: SumTable -> Int -> Int -> IO ()
incre1 ht n maxLen = 
    do  fstates <- H.foldM folder iniState ht
        fstates ()
        where 
            iniState :: () -> IO ()
            iniState () = return ()
            increByKey :: SumState -> IO ()
            increByKey (k@(sum, len), v) = 
                if  len == maxLen -- no point going on
                    then return ()
                    else
                        let k' = (sum + n, len + 1) in
                            do  maybeV <- H.lookup ht k'
                                case    maybeV of
                                        Just o -> H.insert ht k' (o + 1)
                                        Nothing -> H.insert ht k' v
            folder :: (() -> IO ()) -> SumState -> IO (() -> IO ())
            folder k sm =
                let res = \k' -> do k ()
                                    increByKey sm
                    in return res

uniqueSums :: [Int] -> Int -> SumTable -> IO ()
uniqueSums [] _ ht = return ()
uniqueSums (x:xs) maxLen ht = 
    do  incre1 ht x maxLen
        H.insert ht (x, 1) 1
        uniqueSums xs maxLen ht

filterUnique :: [((Int, Int), Int)] -> Int -> [Int]
filterUnique xs l = 
    do  tup@((sum, len), dup) <- xs
        do  True <- return (len == l && dup == 1)
            return sum

main_ :: Int -> Int -> IO ()
main_ max len = 
    let ht' :: IO(SumTable)
        ht' = H.new 
        ints :: [Int]
        ints = [ x^2 | x <- [1..max]] in
            do  ht <- ht'
                uniqueSums ints len ht
                do  xss <- H.toList ht
                    let s = sum $ filterUnique xss len in
                        putStr $ show s

