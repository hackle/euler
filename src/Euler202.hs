module Euler202 where

import Data.List

validPoints :: Integer -> [ Integer ]
validPoints hits = filter (not.hasLeak') points
        where 
            y = (hits + 1) `div` 2 + 1
            startX = if even y then 6 else 3
            cCount = if even y then (y-1) `div` 6 else (y + 1) `div` 6
            points = takeWhile (< y) [ startX + x * 6 | x <- [0..]]
            hasLeak' = hasLeak y

bounces :: Integer -> Integer
bounces hits = 
        let cnt = length $ validPoints hits in
        (toInteger cnt) * (2::Integer)

isValidPoint :: (Integer, Integer) -> Bool
isValidPoint (x, y) = odd y == odd x

isC :: Integer -> Integer -> Bool
isC x y = isValidPoint (x, y) && x `mod` 6 == (if odd y then 3 else 0)

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

findFirstLeakyX :: Integer -> [Integer] -> Integer -> Maybe Integer
findFirstLeakyX y xs y1 = 
    find isLeaking xs
    where
        division = y `div` y1
        isLeaking :: Integer -> Bool
        isLeaking x = x `mod` division == 0 && (isValidPoint (x `div` division, y1))

findLeak :: Integer -> [Integer] -> Integer -> Maybe (Integer, Integer)
findLeak y yFacts x = 
    case find isLeaking yFacts of
        Nothing -> Nothing
        Just fctr -> Just (calcPoint fctr)
    where 
        calcPoint fctr = (y `div` fctr, x `div` fctr)
        isLeaking fctr = x `mod` fctr == 0 && (isValidPoint $ calcPoint fctr)

hasLeak :: Integer -> Integer -> Bool
hasLeak y = (Nothing /=) . (findLeak y $ primeFactorize y)

-- leakPoints :: Integer -> [Integer]
-- leakPoints y = nub $ foldl calcX [] [(y1, x1) | y1 <- facts, x1 <- [1..(y1-1)]]
--     where 
--         facts = factorize y
--         calcX :: [Integer] -> (Integer, Integer) -> [Integer]
--         calcX st (y1, x1) =
--             let valid = 
--                     if isValidPoint x1 y1 
--                         then isC projectedX y 
--                         else False                 
--                 projectedX = (x1 * y `div` y1) in
--                 if valid then insert projectedX st else st


