import Test.Hspec
import Test.QuickCheck
import Euler202
import Data.List

main :: IO ()
main = hspec $ do
    describe "bounces" $ do
        it "satisfies sample" $ do
            bounces 11 `shouldBe` 2
            bounces 7  `shouldBe` 2
            bounces 17 `shouldBe` 0 -- is leaky
            bounces 1000001 `shouldBe` 80840
    describe "factorizes correctly" $ do
        it "works" $ do
            factorize 201235 `shouldBe` [5,167,241,835,1205,40247]
    describe "leaky points" $ do
        it "if point (x1, y1) is leaky on factor y1, then at interval of x1, every point (xi, y1) mapped to (xi', y) on Y is leaky" $ do
            let hits = 100001
                y = (hits + 1) `div` 2 + 1
                startX = if even y then 6 else 3
                cCount = if even y then (y-1) `div` 6 else (y + 1) `div` 6
                ptXs = takeWhile (< y) [ startX + x * 6 | x <- [0..]]
                y1 = last $ factorize y                
                division = y `div` y1
                (Just x1) = findFirstLeakyX y ptXs y1 
                sampling = 15 in
                (take sampling $ leakyPoints 1000001) `shouldBe` (take sampling $ leakyPoints1 1000001)
        -- it "can then result in total leaky points projected from all factor Y" $ do
        --     let hits = 1000001
        --         Analysis { xs = xs, y = y, pointsCount = cCount } = analyze hits in
        --         2 * (cCount - (leakCount y xs)) `shouldBe` 80840
        it "collision - when points from different Y share the same ratio, should calculate from less Y" $ do
            let hits = 1000001
                Analysis { xs = xs, y = y, pointsCount = cCount } = analyze hits 
                y1:y2:_ = reverse $ factorize y
                [div1,div2] = map (y `div`) [y1,y2]
                (Just x1) = findFirstLeakyX y xs y1
                (Just x2) = findFirstLeakyX y xs y2 in
                (findCollisions (x1 `div` div1, y1) (x2 `div` div2, y2)) `shouldBe` [((1, 1), (2,2))]

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