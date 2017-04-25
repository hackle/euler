module Main where

import qualified Lib as L
import System.Environment
import Euler200

main :: IO ()
main = do 
    [cntStr] <- getArgs
    let cnt = read cntStr::Int in
        mapM_ (putStrLn . show) $ take cnt (filter isPrimeProof200 squbes)

    -- [max, maxLen]    <- getArgs
    -- L.main_ (read max::Int) (read maxLen::Int)
