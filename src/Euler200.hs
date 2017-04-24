module Euler200 where 

import qualified Data.HashTable.IO as H

newtype Products = Products [Integer]
instance Eq Products where
    (Products p1@(x1:_)) == (Products p2@(x2:_)) = x1 == x2
instance Ord Products where
    (Products p1@(x1:_)) `compare` (Products p2@(x2:_)) = x1 `compare` x2

type SqubeTable = H.BasicHashTable Integer [Integer]
 
oddsFrom3 :: [Integer]
oddsFrom3 = (let increBy2From x = x:(increBy2From (x+2)) in increBy2From 5)

pst :: [Integer] -> [Integer] -> (Integer, [Integer])
pst ps (y:ys) = 
    if  all (\n -> y `mod` n /= 0) ps
        then (y, ys)
        else pst ps ys

infi :: [Integer] -> [Integer] -> [Integer]
infi taken ns = (let (f,s) = pst taken ns in f:(infi (f:taken) s))

primes :: [Integer]
primes = 2:3:(infi [2, 3] oddsFrom3)

allCombos :: [ [Integer] ]
allCombos = do
    map (fromOne combo32) primes ++ (map (fromOne combo23) primes)
    where 
        combo23 :: Integer -> Integer -> Integer
        combo23 n1 n2 = n1 ^ 2 * n2 ^ 3
        combo32 :: Integer -> Integer -> Integer
        combo32 n1 n2 = n1 ^ 3 * n2 ^ 2
        fromOne :: (Integer -> Integer -> Integer) -> Integer -> [ Integer ]
        fromOne f n = do 
            n1 <- primes
            True <- return (n /= n1)
            return (f n n1)
    
squbes :: [Integer]
squbes = map head allCombos

isPossiblyPrime :: Integer -> Bool
isPossiblyPrime n = 1 == 2 ^ (n - 1) `mod` n