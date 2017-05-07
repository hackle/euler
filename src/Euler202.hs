module Euler202 where

import Data.List

bounces :: Integer -> Integer
bounces hits = 
    let y = (hits + 1) `div` 2 + 1
        startX = if even y then 6 else 3
        cCount = if even y then (y-1) `div` 6 else (y + 1) `div` 6
        cnt = cCount - (toInteger $ length $ (leakPoints y)) in
        -- cnt = length $ filter (not . (hasLeak y)) $ takeWhile (< y) [ startX + x * 6 | x <- [0..]] in
        (toInteger cnt) * (2::Integer)

isValidPoint :: Integer -> Integer -> Bool
isValidPoint x y = odd y == odd x

isC :: Integer -> Integer -> Bool
isC x y = isValidPoint x y && x `mod` 6 == (if odd y then 3 else 0)

primesFrom :: Integer -> [Integer] -> [Integer]
primesFrom n xs = 
    if any (\x -> n `mod` x == 0) xs
        then primesFrom (n + 2) xs
        else n:(primesFrom (n + 2) (n:xs))

primes :: [Integer]
primes = 2:(primesFrom 3 [2])

factorize :: Integer -> [Integer]
factorize n = sort $ nub $ concatMap byP (takeWhile (<= sqroot) primes)
    where
        sqroot = floor $ sqrt (fromInteger n)
        byP p = if n `mod` p == 0 
            then let m = n `div` p in p:m:(factorize m) 
            else []

primeFactorize :: Integer -> [Integer]
primeFactorize n = filter (\x -> n `mod` x == 0) (takeWhile (<= sqroot) primes)
    where
        sqroot = floor $ sqrt (fromInteger n)

hasLeak :: Integer -> Integer -> Bool
hasLeak a = 
    let factsA = primeFactorize a in
        \b ->
            let cds = filter (\x -> b `mod` x == 0) factsA in
                if [] == cds
                    then False
                    else any (\fctr -> isValidPoint (a `div` fctr) (b `div` fctr)) cds

leakPoints :: Integer -> [Integer]
leakPoints y = nub $ foldl calcX [] [(y1, x1) | y1 <- facts, x1 <- [1..(y1-1)]]
    where 
        facts = factorize y
        calcX :: [Integer] -> (Integer, Integer) -> [Integer]
        calcX st (y1, x1) =
            let valid = 
                    if isValidPoint x1 y1 
                        then isC projectedX y 
                        else False                 
                projectedX = (x1 * y `div` y1) in
                if valid then insert projectedX st else st


