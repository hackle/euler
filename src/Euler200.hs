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
        noVariantIsPrime = all (not . isPrime) (variations str len)
        
--https://primes.utm.edu/prove/prove2_3.html
isModule1 :: Integer -> Integer -> Integer -> Bool
isModule1 n d a = a^d `mod` n == 1

isModuleMinus1 :: Integer -> Integer -> Integer -> Integer -> Bool
isModuleMinus1 n d s a = any isMatch [0..(s-1)] 
    where
        isMatch bs =
            let res = foldl (foldOnN n) 1 $ replicate (fromInteger 2^bs) fx in
                elem (res `mod` n) [1, n-1]
        fx = powerMod a d n

getS :: Integer -> Integer 
getS n = last $ takeWhile (\p -> 0 == (n-1) `mod` 2^p) [1..]

getD :: Integer -> Integer -> Integer
getD n s = (n-1) `div` (2^s)

presets = [ (1373653, 2), (25326001, 3), (25000000000, 4), (2152302898747, 5), (3474749660383, 6) ]

fxPwr :: Integer
fxPwr = 2^18
primesAndExps = map (\x -> (x, x^fxPwr)) $ take 7 primes

bases :: Integer -> [Integer]
bases n = 
    let thresh = find (\(n1, _) -> n < n1) presets in
        case thresh of
            Just (_, m) -> take m primes
            Nothing -> take 7 primes

isPrime :: Integer -> Bool
isPrime n = 
    let s = getS n 
        d = getD n s in
            all (\a -> isModuleMinus1 n d s a) (bases n)

foldOnN n st x = ((st `mod` n)* x) `mod` n

powerMod :: Integer -> Integer -> Integer -> Integer
powerMod b p n =
    let res = b^rmd `mod` n * (foldl (foldOnN n) 1 $ replicate cnt fx) in
        res `mod` n
    where 
        fxExp = snd $ head $ filter (\(x,_) -> x == b) primesAndExps
        fx = fxExp `mod` n
        cnt :: Int
        cnt = fromInteger (p `div` fxPwr)
        rmd = p `mod` fxPwr