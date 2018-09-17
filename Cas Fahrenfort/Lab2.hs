
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lecture2

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

-- Exercise 1
-- 1.5 hours
-- The test results are mostly True yet sometimes False. Because the number generation is truly random,
-- you could imagine a scenario where one quartile is completely empty. Because of this, running this test
-- only once is meaningless, and it should be ran many times to estimate the average degree of accuracy. 

-- Runs the probs and countDeviation function with a size of 10000 and returns False when
-- any of the deviations from 2500 are larger than 100.
testProbs :: IO Bool
testProbs = do
                fs <- probs 10000
                ds <- return $ countDeviations fs
                return $ checkProportions 100 ds

-- Returns True if all integers in a list are larger than a supplied integer.
checkProportions :: Int -> [Int] -> Bool
checkProportions p [] = True
checkProportions p (i:is) | i > p     = False
                          | otherwise = checkProportions p is

-- Takes a list of floats, computes how many of the floats fall in each quartile,
-- then computes the deviation from 2500 for each quartile.
-- The 'countQuartiles' function walks the lit of floats, building up an accumulator list 
-- of four integers counting how many of each float are in the corresponding quartile.
countDeviations :: [Float] -> [Int]
countDeviations fs = map deviation $ countQuartiles fs [0,0,0,0]
    where deviation x = abs $ 2500-x
          countQuartiles [] is = is
          countQuartiles (x:xs) (i:j:k:l) | x < 0.25  = countQuartiles xs (i+1:j:k:l)
                                          | x < 0.5   = countQuartiles xs (i:j+1:k:l)
                                          | x < 0.75  = countQuartiles xs (i:j:k+1:l)
                                          | otherwise = countQuartiles xs (i:j:k:[(head l) + 1])

-- Exercise 2
-- 10 min
-- The correctness of this program relies on the mathematical foundations of geometry.
-- The properties of triangles have long since been proven (e.g. Pythagoras about 2000 years ago).
-- However, to solidify the corectness of the program you could run tests with predefined side lengths
-- of which you know the type of triangle they define already.

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c | (a + b < c) || (b + c < a) || (a + c < b) = NoTriangle
               | (a == b) && (b == c)                      = Equilateral
               | (a ^ 2) + (b ^ 2) == (c ^ 2)              = Rectangular
               | (a == b) || (b == c) || (a == c)          = Isosceles
               | otherwise                                 = Other

-- Exercise 3
-- 40 min

data Prop a = Prop (a -> Bool) Int
instance Show (Prop a) where
    show (Prop f n) = show n

prop1, prop2, prop3 :: Prop Int
prop1 = Prop (\x -> even x && x > 3) 1
prop2 = Prop (\x -> even x || x > 3) 2
prop3 = Prop (\x -> (even x && x > 3) || even x) 3

strengthList :: [Prop Int]
strengthList = sortBy sortFunc [prop1, prop2, prop3]
    where sortFunc (Prop f1 n1) (Prop f2 n2) | stronger [(-10)..10] f1 f2 = GT
                                             | otherwise                  = LT

-- Exercise 4
-- 2 hours

-- Assuming no duplicates, this function checks if each element of a exists in b
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = all ((flip elem) b) a && (length a) == (length b)

prop4, prop5 :: Eq a => [a] -> [a] -> Bool
prop4 xs ys = length xs == length ys
prop5 xs ys = all ((flip elem) ys) xs

-- This function runs n tests on a relational property.
-- It generates a list of ints, then takes the first permutation of that list,
-- after which it checks if the property and the function passed to it both
-- resolve to true with the given lists as input.
testPropPerms :: Int -> Int -> ([Int] -> [Int] -> Bool)
                    -> ([Int] -> [Int] -> Bool) -> IO Bool
testPropPerms k n f r = if k == n then return True
                     else do
                      xs <- genIntList
                      ys <- return $ head (permutations xs)
                      if r xs ys && f xs ys then
                        do testPropPerms (k+1) n f r
                      else return False

-- Test isPermutation with prop4 and prop5, 100 tests for each.
testIsPermutation :: IO ()
testIsPermutation = do
                     result1 <- testPropPerms 1 100 isPermutation prop4
                     if (result1) then print ("prop4 passed (100 tests)") else print ("prop4 failed")
                     result2 <- testPropPerms 1 100 isPermutation prop5
                     if (result2) then print ("prop5 passed (100 tests)") else print ("prop5 failed")


-- Exercise 5
-- 1 hour

-- Check whether a list is a derangement of another list i.e. whether it is a permutation and
-- no element of the derangement exists in the same position as in the original list.
isDerangement :: [Int] -> [Int] -> Bool
isDerangement xs ys = isPermutation xs ys && isDerangementRecursion xs ys

isDerangementRecursion :: [Int] -> [Int] -> Bool
isDerangementRecursion [] [] = True
isDerangementRecursion (x:xs) (y:ys) | x == y = False
                                     | otherwise = isDerangementRecursion xs ys

deran :: Int -> [[Int]]
deran n = filter (isDerangement [0..n-1]) (permutations [0..n-1])

-- Return all derangements of a given list.
derangements :: [Int] -> [[Int]]
derangements xs = filter (isDerangement xs) (permutations xs)

-- Tests a property for derangements much like 'testPropPerms' in the previous exercise.
testPropDerans :: Int -> Int -> ([Int] -> [Int] -> Bool)
                    -> ([Int] -> [Int] -> Bool) -> IO Bool
testPropDerans k n f r = 
                     if k == n 
                     then return True
                     else do
                      xs <- genIntList
                      ys <- return $ head' (derangements xs)
                      if ys == [] 
                      then do testPropDerans k n f r
                      else if r xs ys && f xs ys 
                        then do testPropDerans (k+1) n f r
                        else return False

-- The derangement of the empty list, list with one element and list with only equal values is undefined,
-- therefore the 'derangements' function return the empty list. Since Haskell does not like having to give the
-- head of an empty list, here is a function which does return an empty list so the 'testPropDerans' function
-- can operate without crashing.
head' :: [[a]] -> [a]
head' [] = []
head' (x:xs) = x

-- Along with this new property, we can re-use prop4 and prop5 from earlier
prop6 :: [Int] -> [Int] -> Bool
prop6 xs ys = isDerangementRecursion xs ys

-- Test isDerangement with prop4, prop5 and prop6, 100 tests for each.
-- It may take a little time to run all tests.
testIsDerangement :: IO ()
testIsDerangement = do
                     result1 <- testPropDerans 1 100 isDerangement prop4
                     if (result1) then print ("prop4 passed (100 tests)") else print ("prop4 failed")
                     result2 <- testPropDerans 1 100 isDerangement prop5
                     if (result2) then print ("prop5 passed (100 tests)") else print ("prop5 failed")
                     result3 <- testPropDerans 1 100 isDerangement prop6
                     if (result3) then print ("prop6 passed (100 tests)") else print ("prop6 failed")

-- Exercise 6
-- 30 mins

-- Specification: for all letters in a string, they should be replaced by the 13th letter after it in the alphabet,
-- wrapping around back to 'a' after 'z'.

rot13 :: [Char] -> [Char]
rot13 line = map f line
    where f c | (ord c) + 13 > 122 = toEnum $ 96 + (13 - (122 - (ord c)))
              | otherwise = toEnum $ (ord c) + 13

-- Compares both arguments to see if the rot13 algorithm specification holds on the second one
-- with respect to the first one. It walks both lists recursively and compares its character values.
prop7 :: [Char] -> [Char] -> Bool
prop7 [] [] = True
prop7 (x:xs) (y:ys) | checkValues x y = prop7 xs ys
                    | otherwise = False
    where checkValues c1 c2 | ord c1 <= 109 = ord c1 + 13 == ord c2
          checkValues c1 c2 | ord c1 > 109  = ord c2      == 96 + (13 - (122 - ord c1))

-- Give a random char between 'a' and 'z'.
getRandomChar :: IO Char
getRandomChar = do 
                i <- getRandomInt 25
                return $ toEnum (97 + i)

-- Give a list of random chars with length n.
getRandomCharList :: Int -> IO [Char]
getRandomCharList 0 = return []
getRandomCharList n = do 
    x <- getRandomChar
    xs <- getRandomCharList (n-1)
    return (x:xs)

-- Runs a test for rot13 n times.
-- It generates a random character list with a random length between 0 and 100.
-- Then applies the rot13 algorithm to it and checks the altered string with prop7 to
-- see if it satisfies the postconditions of rot13.
testPropRot13 :: Int -> Int -> IO Bool
testPropRot13 k n = 
             if k == n 
             then return True
             else do
              i <- getRandomInt 100
              rnd <- getRandomCharList i
              rot <- return $ rot13 rnd
              if prop7 rnd rot
                then do testPropRot13 (k+1) n
                else return False

testRot13 :: IO ()
testRot13 = do 
             result1 <- testPropRot13 1 100
             if (result1) then print "prop7 passed (100 tests)" else print "prop7 failed"

-- Exercise 7
i :: [Char]
i = ['G','B','8','2','W','E','S','T','1','2','3','4','5','6','9','8','7','6','5','4','3','2']

iban :: String -> [Int]
iban (i:j:k:l:m) = toSingles (map replaceChars $ m ++ [i,j,k,l])
    where replaceChars c | ord c >= 65 && ord c <= 90 = ord c - 55
                         | ord c >= 97 && ord c <= 122 = ord c - 87
                         | otherwise = ord c - 48

toSingles :: [Int] -> [Int]
toSingles xs = f xs []
    where f [] ys = ys
          f (x:xs) ys | x >= 10 = f xs (ys ++ [x `div` 10, x `mod` 10])
                      | otherwise = f xs (ys ++ [x])