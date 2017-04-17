module Main where

import qualified Lib as L
import System.Environment

main :: IO ()
main = do
    [max, maxLen]    <- getArgs
    L.main_ (read max::Int) (read maxLen::Int)
