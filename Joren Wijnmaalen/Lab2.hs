
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lecture2

-- infix 1 --> 

-- (-->) :: Bool -> Bool -> Bool
-- p --> q = (not p) || q

-- stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
-- stronger xs p q = (flip all) xs (\ x -> p x --> q x)
-- weaker   xs p q = stronger xs q p 

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)


-- EXERCISE 1 -- 
-- 1h
-- The test tests arbitrarely if the margin of error is smaller than 100. There is no "good" test however, since
-- an independent random function is tested. 

-- Takes 10000 random numbers, counts the quartiles and checks if the margin of error is smaller than 100
-- for all quartiles
testProbs :: IO Bool
testProbs = do
    fs <- probs 10000
    return $ all (<100) (quartileDeviation (divideToQuartiles fs [0,0,0,0]))

-- Subtracts 2500 of the quartiles to see what the margin of error is.
quartileDeviation :: [Int] -> [Int]
quartileDeviation xs = map (abs . (2500 -)) xs

-- Increments the counter of the quartile if the element falls into it.
divideToQuartiles :: [Float] -> [Int] -> [Int]
divideToQuartiles [] acc = acc
divideToQuartiles (x:xs) (f:s:t:fr) | x <= 0.24 = divideToQuartiles xs ((f+1):s:t:fr)
                                    | x <= 0.49 = divideToQuartiles xs (f:(s+1):t:fr)
                                    | x <= 0.74 = divideToQuartiles xs (f:s:(t+1):fr)
                                    | x <= 1 = divideToQuartiles xs (f:s:t:[((head fr) +1)])

-- EXCERCISE 2 --
-- 5 min
-- The correctness of this program is proven by the mathematical properties of triangles.

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c  | a + b < c || b + c < a || c + a < b = NoTriangle
                | (a == b) && (b == c) = Equilateral
                | a ^ 2 + b ^ 2 == c ^ 2 = Rectangular
                | (a == b) || (b == c) || (c == a) = Isosceles
                | otherwise = Other

-- EXERCISE 3 --
-- 30 min
-- answer: [3,2,1]

-- The prop datatype is essentially a tuple of the prop function with a number as label
-- for printing purposes
data Prop a = Prop (a -> Bool) Int 
instance Show (Prop a) where
    show (Prop f n) = show n

-- Order the properties using the orderfunction
strengthList :: [Prop Int]
strengthList = sortBy ordFunc [prop1, prop2, prop3]

-- This order function defines an ordering based on the stronger function between two properties
ordFunc :: Prop Int -> Prop Int -> Ordering
ordFunc (Prop f n) (Prop f2 n2)   | stronger [1..10] f f2 = GT
                                    | otherwise = LT

-- The three props from the exercise
prop1, prop2, prop3 :: Prop Int
prop1 = Prop (\x -> even x && x > 3) 1
prop2 = Prop (\x -> even x || x > 3) 2
prop3 = Prop (\x -> (even x && x > 3) || even x) 3

-- Exercise 4 --
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = all ((flip elem) b) a && length a == length b

-- This testiterator takes a relation property to the the precondition of the isPermutation function.
-- Because permutations are randomly generated, it is sure that the pre conditions hold. If the preconditions
-- hold, the isPermutation function should always generate True, because we know the input is valid. 

sameLengthProp :: [a] -> [a] -> Bool
sameLengthProp a b = length a == length b


testIterator :: Int -> Int -> ([Int] -> [Int] -> Bool) -> ([Int] -> [Int] -> Bool) -> IO ()
testIterator k n f r = if k == n then print (show n ++ " tests passed")
 else do
    xs <- genIntList
    ys <- return $ head (permutations xs)
    if r xs ys --> f xs ys then do
        print ("pass on: " ++ show xs)
        testIterator (k+1) n f r
    else error ("failed test on: " ++ show xs)

-- testPermutations :: [RelationProp [a]] -> (a -> a) -> a -> [RelationProp [a]] -> Bool
-- testPermutations precondition f input postcondition = all (==True) $ map (mapFunc input) precondition
--     where mapFunc x (RelationProp f) = f input

-- EXERCISE 5 --
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement (x:xs) (y:ys) | x == y = False
                            | otherwise = isDerangement xs ys

deran :: Int -> [[Int]]
deran n = filter (isDerangement [0..n-1]) ( permutations [0..n-1])

-- derangeProp :: [a] -> [a] -> Bool
-- derangeProp xs ys = all ((flip elem) xs) ys

-- derangeProp2 :: [a] -> [a] -> Bool
-- derangeProp2 xs ys = isPermutation xs ys

