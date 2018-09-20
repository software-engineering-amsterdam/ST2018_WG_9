
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lecture2

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)
testProbs :: IO Bool
testProbs = do
    xs <- probs 10000
    return (all (<100) (deviations (groupNumbers xs [0,0,0,0])))

groupNumbers :: [Float]-> [Int]->[Int] 
groupNumbers [] ys = ys 
groupNumbers (x:xs) (fi:se:th:fo) 
    | x < 0.25 = groupNumbers xs ((fi+1):se:th:fo)
    | x < 0.50 = groupNumbers xs (fi:(se+1):th:fo)
    | x < 0.75 = groupNumbers xs (fi:se:(th+1):fo)
    | otherwise = groupNumbers xs (fi:se:th:[((head fo) + 1)])

deviations xs = map (2500 -) xs

-- 60min

-- Because its the numbers that are generated are random,
-- it doesnt matter what maximum deviation we set.
-- Eventually it will always fail. Because its random.

triangle :: Int -> Int -> Int -> Shape
triangle a b c 
                | (a + b < c) || (b + c < a) || (a + c < b)                         = NoTriangle
                | (a == b) && (b == c)                                              = Equilateral
                | (a*a + b*b == c*c) || (c*c + b*b == a*a) || (a*a + c*c == b*b)    = Rectangular
                | (a == b) || (b == c) || (c == a)                                  = Isosceles
                | otherwise                                                         = Other
-- 15min

data Prop a = Prop (a -> Bool) Int
instance Show (Prop a) where
    show(Prop f n) = show n


prop1, prop2, prop3 :: Prop Int
prop1 = Prop (\x ->even x && x > 3) 1
prop2 = Prop (\x -> even x || x > 3) 2
prop3 = Prop (\x -> (even x && x > 3) || even x) 3
prop3' = Prop (\x -> ((even x && x > 3) || even x )) 4

orderFunction :: Prop Int -> Prop Int -> Ordering
orderFunction (Prop f1 _) (Prop f2 _)   | stronger [1..10] f1 f2 && stronger [1..10] f2 f1 = EQ
                                        | stronger [1..10] f1 f2 = GT
                                        | stronger [1..10] f2 f1 = LT

orderProperties = sortBy orderFunction [prop1, prop2, prop3, prop3']                    
--30min

isPermutation :: (Eq a) => [a] -> [a] -> Bool
isPermutation xs ys = all ((flip elem) xs) ys && length xs == length ys

prop4 xs ys = length xs == length ys
prop5 xs ys = all ((flip elem) xs) ys

isDerangement :: [Int] -> [Int] -> Bool
isDerangement xs ys = isPermutation xs ys && derangeProp xs ys

derangeProp :: [Int] -> [Int] -> Bool
derangeProp [] [] = True
derangeProp (x:xs) (y:ys) | x == y = False
                      | otherwise = derangeProp xs ys

deran :: Int -> [[Int]]
deran n = filter (isDerangement [0..n-1]) (permutations [0..n-1])

-- testPermu :: Int -> Int -> ([Int] -> [Int])-> ([Int] -> [Int] -> Bool) -> IO ()
-- testPermu k n r = if k == n then print (show n ++ " tests passed")
--                 else do
--                   xs <- genIntList
--                   ys <- (permutations . head) xs
--                   if isPermutation xs ys && r ys then
--                     do print ("pass on: " ++ show xs)
--                        testPermu (k+1) n r
--                   else error ("failed test on: " ++ show xs)