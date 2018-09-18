
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
-- 2 hours
-- The following tests are written with the following logic:
-- If the function to be tested returns true, the relational property between the input and output should also yield true.
-- This is enforced with logical implication (-->)
--
-- Relational properties and Preconditionals are the only testable properties with this type of function.
-- This is because the function to be tested returns only in the following domain: (True, False)
-- There are no unary properties that describe binary output.

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = all ((flip elem) b) a && length a == length b

-- This testiterator takes a relation property to the the precondition of the isPermutation function.
-- Because permutations are randomly generated, it is sure that the pre conditions hold. If the preconditions
-- hold, the isPermutation function should always generate True, because we know the input is valid. 

-- Input and output of the function should have the same length
sameLengthProp :: Eq a => [a] -> [a] -> Bool
sameLengthProp a b = length a == length b

-- Every element of the input should be present in the output
allElementsPresent :: Eq a => [a] -> [a] -> Bool
allElementsPresent a b = all ((flip elem) b) a

-- k, current test
-- n, total amount of tests
-- f, function to be tested
-- r, relation property
testIterator :: Int -> Int -> ([Int] -> [Int] -> Bool) -> ([Int] -> [Int] -> Bool) -> IO ()
testIterator k n f r = if k == n then print (show n ++ " tests passed")
 else do
    xs <- genIntList
    ys <- return $ head (permutations xs)
    if f xs ys --> r xs ys then do
        print ("pass on: " ++ show xs)
        testIterator (k+1) n f r
    else error ("failed test on: " ++ show xs)

-- Runs the tests with, for example, the sameLenghtProp
-- Returns: 100 tests passed
manualTestRunner :: IO ()
manualTestRunner = testIterator 1 100 isPermutation sameLengthProp

-- Test implementation using quickcheck
-- r is the relational property to be tested
quickCheckPermutations :: [Int] -> ([Int] -> [Int] -> Bool) -> Bool
quickCheckPermutations xs r = isPermutation xs ys --> r xs ys
        where ys = head $ permutations xs

-- Returns: +++ OK, passed 100 tests
quickCheckPermRunner :: IO ()
quickCheckPermRunner = quickCheck $ (flip quickCheckPermutations) allElementsPresent

-- EXERCISE 5 --
-- 1 hour --
-- isDerangement is essentialy an extension on the isPermutations function. Therefore the relational properties
-- defined for isPermutations also apply to isDerangement.

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys && isDerangement' xs ys
    where   isDerangement' [] [] = True
            isDerangement' (x:xs) (y:ys)    | x == y = False
                                            | otherwise = isDerangement' xs ys

deran :: Int -> [[Int]]
deran n = filter (isDerangement [0..n-1]) ( permutations [0..n-1])

-- Input and output can't be the same
derangeProp :: (Num a, Eq a) => [a] -> [a] -> Bool
derangeProp xs ys = xs /= ys

-- TODO: Come up with more props

-- Derangements of [1,2,3]
trueDerangements :: (Num a, Eq a) => [[a]]
trueDerangements = [[1,2,0], [2,0,1]]

falseDerangements :: (Num a, Eq a) => [[a]]
falseDerangements = [[1,2,3], [1,2], [1,3,2]]

-- orig: The list where the derangements originate from
-- ders: The list of derangements
-- r: the relational property
manualDerangeTest :: (Num a, Eq a) => [a] -> [[a]] -> ([a] -> [a] -> Bool) -> Bool
manualDerangeTest orig ders r = all (==True) ys
        where ys = map (\x -> isDerangement x orig --> r orig x) ders

manualDerangeTestRunner :: Bool
manualDerangeTestRunner = manualDerangeTest [1,2,3] falseDerangements derangeProp

quickCheckDerange :: Int -> Bool
quickCheckDerange n = n > 0  && n < 10 --> manualDerangeTest [0..n-1] (deran n) derangeProp

quickCheckDerangeRunner :: IO ()
quickCheckDerangeRunner = quickCheck quickCheckDerange


-- EXERCISE 6 --
-- 45 mins --
-- Formal Specification
-- Preconditions:   - The input should be a string without numbers, all lowercase
-- Postconditions:  - Each letter in the string should be substituted by the letter 13 places through the alphabet
--                  - The output should be a lowercased string excluded from numbers
--                  - rot13 . rot13 $ x = x

alphabet :: [Char]
alphabet = "abcdefghijklmnopqrstuvwxyz"

indexOf :: Eq a => a -> [a] -> Int
indexOf e xs = indexOf' e 0 xs
    where   indexOf' e n [] = -1
            indexOf' e n (x:xs) | e == x = n
                                | otherwise = indexOf' e (n+1) xs

rot13 :: [Char] -> [Char]
rot13 xs = map (\x -> alphabet !! ((alphIndex x + 13) - 26 * ((alphIndex x + 13) `div` 26))) xs
        where alphIndex x = indexOf x alphabet
    

rot13Test1 :: [Char] -> Bool
rot13Test1 xs = (all (\x -> (indexOf x alphabet) /= -1) xs) --> xs == (rot13 . rot13 $ xs)

rot13Test2 :: [Char] -> Bool
rot13Test2 xs = (all (\x -> (indexOf x alphabet) /= -1) xs) --> length xs == (length . rot13 $ xs)
--    alphabet !! ((indexOf x alphabet) + 13)) xs

rot13TestRunner :: IO ()
rot13TestRunner = quickCheck rot13Test1 -- or rot13Test2

-- EXERCISE 7 --
iban :: String -> Bool
iban xs = (substituteLetters . moveFirstFour $ xs) `mod` 97 == 1

substituteLetters :: String -> Integer
substituteLetters xs = read $ intercalate "" $ filter (/= " ") $ letterToNumber xs :: Integer
        where   letterToNumber xs = map (\x -> if alphIndex x /= -1 then show $ 10 + alphIndex x else [x]) xs 
                alphIndex x = indexOf x alphabet'


moveFirstFour :: String -> String
moveFirstFour (a:b:c:d:e) = e ++ [a, b, c, d]

alphabet' :: String
alphabet' = ['A'..'Z']

-- List of valid IBANs from https://www.iban.com/structure
validIban :: [String]
validIban = ["AL35202111090000000001234567","AD1400080001001234567890","AT483200000012345864","AZ96AZEJ00000000001234567890","BH02CITI00001077181611",
"BY86AKBB10100000002966000000","BE71096123456769","BA393385804800211234","BR1500000000000010932840814P2","BG18RZBB91550123456789",
"CR23015108410026012345","HR1723600001101234565","CY21002001950000357001234567","CZ5508000000001234567899","DK9520000123456789",
"DO22ACAU00000000000123456789","SV43ACAT00000000000000123123","EE471000001020145685","FO9264600123456789","FI1410093000123458",
"FR7630006000011234567890189","GE60NB0000000123456789","DE91100000000123456789","GI04BARC000001234567890","GR9608100010000001234567890",
"GL8964710123456789","GT20AGRO00000000001234567890","HU93116000060000000012345676","IS030001121234561234567890","IQ20CBIQ861800101010500",
"IE64IRCE92050112345678","IL170108000000012612345","IT60X0542811101000000123456","JO71CBJO0000000000001234567890","KZ563190000012344567",
"XK051212012345678906","KW81CBKU0000000000001234560101","LV97HABA0012345678910","LB92000700000000123123456123","LI7408806123456789012",
"LT601010012345678901","LU120010001234567891","MK07200002785123453","MT31MALT01100000000000000000123","MR1300020001010000123456753",
"MU43BOMM0101123456789101000MUR","MD21EX000000000001234567","MC5810096180790123456789085","ME25505000012345678951","NL02ABNA0123456789",
"NO8330001234567","PK36SCBL0000001123456702","PS92PALS000000000400123456702","PL10105000997603123456789123","PT50002700000001234567833",
"QA54QNBA000000000000693123456","RO09BCYP0000001234567890","LC14BOSL123456789012345678901234","SM76P0854009812123456789123","ST23000200000289355710148",
"SA4420000001234567891234","RS35105008123123123173","SC52BAHL01031234567890123456USD","SK8975000000000012345671","SI56192001234567892","ES7921000813610123456789",
"SE7280000810340009783242","CH5604835012345678009","TL380010012345678910106","TN5904018104004942712345","TR320010009999901234567890","UA903052992990004149123456789",
"AE460090000000123456789","GB98MIDL07009312345678","VG21PACG0000000123456789"]
