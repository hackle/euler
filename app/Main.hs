module Main where

import qualified Lib as L
import System.Environment
import Euler200

main :: IO ()
main = do mapM_ (putStrLn . show) $ take 10 (filter isPrimeProof200 squbes)

    -- [max, maxLen]    <- getArgs
    -- L.main_ (read max::Int) (read maxLen::Int)
