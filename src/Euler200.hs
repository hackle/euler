module Euler200 where 
 
oddsFrom3 :: [Int]
oddsFrom3 = (let increBy2From x = x:(increBy2From (x+2)) in increBy2From 5)

pst :: [Int] -> [Int] -> (Int, [Int])
pst ps (y:ys) = 
    if  all (\n -> y `mod` n /= 0) ps
        then (y, ys)
        else pst ps ys

infi :: [Int] -> [Int] -> [Int]
infi taken ns = (let (f,s) = pst taken ns in f:(infi (f:taken) s))

primes :: [Int]
primes = infi [2, 3] oddsFrom3