module Euler200 where 

import Data.List

newtype Products = Products [Integer] deriving (Show)
instance Eq Products where
    (Products p1@(x1:_)) == (Products p2@(x2:_)) = x1 == x2
instance Ord Products where
    (Products p1@(x1:_)) `compare` (Products p2@(x2:_)) = x1 `compare` x2
 
oddsFrom3 :: [Integer]
oddsFrom3 = 2:[3,5..]

pstate :: [Integer] -> [Integer] -> (Integer, [Integer])
pstate ps (y:ys) = 
    if  all (\n -> y `mod` n /= 0) ps
        then (y, ys)
        else pstate ps ys

infi :: [Integer] -> [Integer] -> [Integer]
infi taken ns = (let (f,s) = pstate taken ns in f:(infi (f:taken) s))

primes :: [Integer]
primes = infi [] oddsFrom3

allCombos :: [ Products ]
allCombos = do
    map (\x -> Products $ fromOne combo32 x) primes
    where
        combo32 :: Integer -> Integer -> Integer
        combo32 n1 n2 = n1 ^ 3 * n2 ^ 2
        fromOne :: (Integer -> Integer -> Integer) -> Integer -> [ Integer ]
        fromOne f n = do 
            n1 <- primes
            True <- return (n /= n1)
            return (f n n1)
    
takeOnce :: [Products] -> [Integer]
takeOnce src@(p1:ss) = p1Head:(takeOnce src')
    where
        (Products (p1Head:p1Tail)) = p1
        src' = insert (Products p1Tail) $ delete p1 src

squbes :: [Integer]
squbes = takeOnce allCombos

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

contains200 :: Integer -> Bool
contains200 n = isInfixOf "200" (show n)

isPrimeProof200 :: Integer -> Bool
isPrimeProof200 n = contains200 n && noVariantIsPrime
    where
        str = show n
        len = length str
        noVariantIsPrime = all (not . isPossiblyPrime) (variations str len)
        
--https://primes.utm.edu/prove/prove2_3.html
isPrime :: Integer -> Bool
isPrime n = any (\a -> isModule1 a || isModuleMinus1 a) bases
    where
        isModule1 a = a^d `mod` n == 1
        isModuleMinus1 a = any (\r -> (a^(d*2^r)) `mod` n == n - 1) [0..(s-1)]
        s = last $ takeWhile (\p -> 0 == (n-1) `mod` 2^p) [1..]
        d = (n-1) `div` (2^s)
        bases = take 7 primes