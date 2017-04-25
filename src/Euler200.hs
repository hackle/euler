module Euler200 where 

import Data.List

newtype Products = Products [Integer] deriving (Show)
instance Eq Products where
    (Products p1@(x1:_)) == (Products p2@(x2:_)) = x1 == x2
instance Ord Products where
    (Products p1@(x1:_)) `compare` (Products p2@(x2:_)) = x1 `compare` x2
 
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
    map (fromOne combo32) primes
    where
        combo32 :: Integer -> Integer -> Integer
        combo32 n1 n2 = n1 ^ 3 * n2 ^ 2
        fromOne :: (Integer -> Integer -> Integer) -> Integer -> [ Integer ]
        fromOne f n = do 
            n1 <- primes
            True <- return (n /= n1)
            return (f n n1)
    
type OrderState = ([Integer], [Products], Integer, [Products])

orderOnce :: OrderState -> OrderState
orderOnce ([], [], _, src@((Products s):ss)) =
    ([], [Products s], sh, src')
    where
        (sh:_) = s
        src' = delete (Products s) src
orderOnce (xs, taken@(p1@(Products (th:tt)):ts), maxHead, src@((Products s):ss)) = (xs', taken', maxHead', src')
    where
        minHead = th
        takeNext = minHead == maxHead
        maxHead' = if takeNext then (head s) else maxHead
        xs' = [minHead]
        taken' = 
            let updated = insert (Products tt) $ delete p1 taken in
                if takeNext then insert (Products s) updated  else updated
        src' = if takeNext then ss else src

squbes :: [Integer]
squbes = concatMap fsts $ iterate orderOnce ([], [], 0, allComboProducts)
    where
        fsts (h, _, _, _) = h
        allComboProducts = map (\xs -> Products xs) allCombos

isPossiblyPrime :: Integer -> Bool
isPossiblyPrime n = 1 == 2 ^ (n - 1) `mod` n

easyDigits = ['0','2','4','5','6','8']
toughDigits = ['1','3','7','9']
allDigits = ['0' .. '9']

replaceAt str len idx =
    let digits = delete (str!!idx) $ if idx == len - 1 then toughDigits else allDigits 
        replaceOne d = (take idx str) ++ [d] ++ (drop (idx + 1) str)
        in map replaceOne digits

variations str len =
    let inits = init str
        vs = if elem (last str) easyDigits
                then map (\x -> inits++[x]) toughDigits
                else concatMap (replaceAt str len) [0..(len - 1)]
        in map (\s -> read s::Integer) vs


isPrimeProof200 :: Integer -> Bool
isPrimeProof200 n =
    contains200 && noVariantIsPrime
    where
        str = show n
        len = length str
        contains200 = isInfixOf "200" str
        noVariantIsPrime = all (not . isPossiblyPrime) (variations str len)
        
