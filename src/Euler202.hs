module Euler202 where

import Data.List

data Analysis = Analysis { y::Integer, yFactors::[Integer], startX::Integer, pointsCount::Integer, xs::[Integer] }
type Point = (Integer, Integer)

countPoints :: Integer -> Integer -> Integer
countPoints y interval =
    if even y then (y-1) `div` interval else (y + 1) `div` interval

analyze :: Integer -> Analysis
analyze hits = Analysis { y = y, yFactors = yFactors, startX = startX, pointsCount = pointsCount, xs = xs }
    where 
        y = (hits + 1) `div` 2 + 1
        yFactors = reverse $ factorize y
        startX = if even y then 6 else 3
        pointsCount = countPoints y 6
        xs = takeWhile (< y) [ startX + x * 6 | x <- [0..]]

validPoints :: Integer -> [ Integer ]
validPoints hits = filter (not.hasLeak') xs
        where 
            Analysis { xs = xs, y = y } = analyze hits
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
primeFactorize n = filter (flip elem primes') $ factorize n 
    where 
        sqroot = floor $ sqrt (fromInteger n)
        primes' = (takeWhile (<= sqroot) primes)


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

leakyPoints :: Integer -> [ Integer ]
leakyPoints hits = filter hasLeak' xs
        where 
            Analysis { xs = xs, y = y } = analyze hits
            hasLeak' = hasLeak y

leakyPoints1 :: Integer -> [Integer]
leakyPoints1 hits =
    let Analysis { xs = xs, y = y } = analyze hits in
        concatMap (leaksOnY y xs) $ (reverse $ factorize y)

leaksOnY :: Integer -> [Integer] -> Integer -> [Integer]
leaksOnY y ptXs y1 = 
    let fstX = findFirstLeakyX y ptXs y1 in
        case fstX of 
            Nothing -> []
            Just x1 ->
                let interval = if even y1 then x1 else x1 * 2 in
                    takeWhile (< y) $ iterate (+ interval) x1

leakCount :: Integer -> [Integer] -> Integer
leakCount y ptXs = toInteger $ length $ foldl (\st y1 -> union st $ leaksOnY y ptXs y1) [] (factorize y)

leakCountOnY :: Integer -> [Integer] -> Integer -> Integer
leakCountOnY y ptXs y1 = 
    case findFirstLeakyX y ptXs y1 of
        Nothing -> 0
        Just x1 -> 
            let interval = if even y1 then x1 else x1 * 2 in
                if even y then ((y-1) `div` interval) else ((y + 1) `div` interval)

bounces1 hits =
    let Analysis { xs = xs, y = y, pointsCount = cCount } = analyze hits in
        2 * (cCount - (leakCount y xs))

bounces2 hits = 
    (cCount - (toInteger $ length $ filter isLeaky xs)) * 2
    where
        Analysis { xs = xs, y = y, pointsCount = cCount, yFactors = yFactors } = analyze hits
        check1 y1 x' =             
            case findFirstLeakyX y xs y1 of
                Nothing -> False
                Just x1 ->
                    let interval = intervalX x1 y1 in
                        (x' - x1) `mod` interval == 0                            
        checks = map check1 yFactors
        isLeaky x = any (\chk -> chk x) checks


type CollisionState = (Integer, [Integer])

allLeaks :: Analysis -> Integer
allLeaks analysis@(Analysis { xs = xs, yFactors = yFactors, y = y, pointsCount = cCount }) =
    fst $ foldOne (0, yFactors) (head yFactors)
    where
        foldOne :: CollisionState -> Integer -> CollisionState
        foldOne st@(cnt, _:ys) y1 = (cnt + leaksNow - collided, ys)
            where
                leaksNow = leakTotalOnY analysis y1
                collided = sum $ map (collisionCounts analysis y1) ys

leakTotalOnY :: Analysis -> Integer -> Integer
leakTotalOnY analysis@(Analysis { xs = xs, yFactors = yFactors, y = y, pointsCount = cCount }) y1 =
    case findFirstLeakyX y xs y1 of
        Nothing -> 0
        Just x1 -> countPoints y1 (intervalX (x1 `div` div1) y1)
    where
        div1 = y `div` y1

collisionCounts :: Analysis -> Integer -> Integer -> Integer
collisionCounts (Analysis { y = y, yFactors = yFactors, xs = xs }) y1 y2 =    
    case firstCollision of
        Nothing -> 0
        Just colX -> colX
    where
        firstCollision :: Maybe Integer
        firstCollision = do
            x1 <- findFirstLeakyX y xs y1
            x2 <- findFirstLeakyX y xs y2
            let interval1 = intervalX x1 y1
                interval2 = intervalX x2 y2
                lcmn = lcm interval1 interval2 
                divlcm1 = lcmn `div` interval1 in
                return (countPoints y1 (intervalX divlcm1 y1))

collisions :: Analysis -> Integer -> Integer -> [(Point, Point)]
collisions (Analysis { y = y, yFactors = yFactors, xs = xs }) y1 y2 = 
    case firstCollision of
        Nothing -> []
        Just pts -> pts
    where
        firstCollision :: Maybe [(Point, Point)]
        firstCollision = do
            x1 <- findFirstLeakyX y xs y1
            x2 <- findFirstLeakyX y xs y2
            let [div1, div2] = map (y `div`) [y1, y2]
                interval1 = intervalX x1 y1
                interval2 = intervalX x2 y2
                lcmn = lcm interval1 interval2 
                [divlcm1, divlcm2] = map (lcmn `div`) [interval1, interval2]
                xs1 = iterate (+ (intervalX divlcm1 y1)) divlcm1
                xs2 = iterate (+ (intervalX divlcm2 y2)) divlcm2
                pts1 = map (\x' -> (x', y1)) xs1
                pts2 = map (\x' -> (x', y2)) xs2 in
                return (zip pts1 pts2)
                        
intervalX :: Integer -> Integer -> Integer
intervalX x y = if even y then x else x * 2

formulaX :: Integer -> Integer -> (Integer -> Bool)
formulaX startX y x1 =
    0 == (x1 - startX) `mod` interval
    where
        interval = intervalX startX y    

repeatOnY :: Integer -> Integer -> [Integer]
repeatOnY x y = 
    let interval = if even y then x else x * 2 in
        takeWhile (< y) $ iterate (+ interval) x

ratioEqual :: Point -> Point -> Bool
ratioEqual (x1, y1) (x2, y2) = 
    map (`div` gcd1) [x1, y1] == map (`div` gcd2) [x2, y2]
    where
        gcd1 = gcd x1 y1
        gcd2 = gcd x2 y2

findCollisions :: Point -> Point -> [(Point, Point)]
findCollisions greater@(x1, y1) smaller@(x2, y2) = 
        do  x2' <- x2s
            x1' <- x1s
            True <- return (ratioEqual (x1', y1) (x2', y2))
            return ((x1', y1), (x2', y2))
        where
            x1s = repeatOnY x1 y1
            x2s = repeatOnY x2 y2