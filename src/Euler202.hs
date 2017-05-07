module Euler202 where

bounces :: Integer -> Integer
bounces hits = 
    let y = (hits + 1) `div` 2 + 1
        startX = if y `mod` 2 == 0 then 6 else 3
        cnt = length $ filter (noLeak y) $ takeWhile (< y) [ startX + x * 6 | x <- [0..]] in
        (toInteger cnt) * (2::Integer)

isValidPoint :: Integer -> Integer -> Bool
isValidPoint x y = y `mod` 2 == x `mod` 2

factorize :: Integer -> [Integer]
factorize n = filter (\x -> n `mod` x == 0) [2..hlf]
    where hlf = max 2 (n `div` 2)

noLeak :: Integer -> Integer -> Bool
noLeak a b = 
    if 1 == gcdn
        then True
        else not $ any (\fctr -> isValidPoint (a `div` fctr) (b `div` fctr)) (factorize gcdn)
    where gcdn = gcd a b