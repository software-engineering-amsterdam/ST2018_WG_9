
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
-- The correctness of this program relies on the mathematical foundations of geometry.
-- The properties of triangles have long since been proven (e.g. Pythagoras about 2000 years ago).
-- However, to solidify the corectness of the program you could run tests with predefined side lengths
-- of which you know the type of triangle they define already.

triangle :: Int -> Int -> Int -> Shape
triangle a b c  | (a + b < c) || (b + c < a) || (a + c < b)                         = NoTriangle
                | (a == b) && (b == c)                                              = Equilateral
                | (a*a + b*b == c*c) || (c*c + b*b == a*a) || (a*a + c*c == b*b)    = Rectangular
                | (a == b) || (b == c) || (c == a)                                  = Isosceles
                | otherwise                                                         = Other

-- Exercise 3
-- When drawing the properties as Venn diagrams, we get the order 2,1,4,3 where 1 and 4 are equal.
-- We get the same answer when running the function 'strengthList': [2,1,4,3]

-- The prop datatype is essentially a tuple of the prop function with a number as label
-- for printing purposes
data Prop a = Prop (a -> Bool) Int 
instance Show (Prop a) where
    show (Prop f n) = show n

-- Order the properties using 'ordFunc'.
-- Changing the order of the properties in the list may change the answer from [2,1,4,3] to [2,4,1,3]
-- since 1 and 4 are equal.
strengthList :: [Prop Int]
strengthList = sortBy ordFunc [prop1, prop2, prop3, prop4]

-- This order function defines an ordering based on the stronger function between two properties
ordFunc :: Prop Int -> Prop Int -> Ordering
ordFunc (Prop f n) (Prop f2 n2) | stronger [1..10] f f2 && stronger [1..10] f2 f = EQ
                                | stronger [1..10] f f2 = LT
                                | otherwise = GT

-- The three props from the exercise
prop1, prop2, prop3, prop4 :: Prop Int
prop1 = Prop (\x -> even x) 1
prop2 = Prop (\x -> even x && x > 3) 2
prop3 = Prop (\x -> even x || x > 3) 3
prop4 = Prop (\x -> (even x && x > 3) || even x) 4

-- Exercise 4
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
allElementsProp :: Eq a => [a] -> [a] -> Bool
allElementsProp a b = all ((flip elem) b) a

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
manualPermsTest :: IO ()
manualPermsTest = testIterator 1 100 isPermutation sameLengthProp

-- Test implementation using quickcheck
-- r is the relational property to be tested
quickCheckPermutations :: [Int] -> ([Int] -> [Int] -> Bool) -> Bool
quickCheckPermutations xs r = isPermutation xs ys --> r xs ys
        where ys = head $ permutations xs

-- Returns: +++ OK, passed 100 tests
quickCheckPermsRunner :: IO ()
quickCheckPermsRunner = quickCheck $ (flip quickCheckPermutations) allElementsProp

-- Exercise 5
-- Check whether a list is a derangement of another list i.e. whether it is a permutation and
-- no element of the derangement exists in the same position as in the original list.

isDerangement :: [Int] -> [Int] -> Bool
isDerangement xs ys = isPermutation xs ys && isDerangement' xs ys
        where isDerangement' [] [] = True
              isDerangement' (x:xs) (y:ys) | x == y = False
                                           | otherwise = isDerangement' xs ys

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

-- Along with these new properties, we can re-use sameLengthProp and allElementsProp from earlier.

-- Check if no two equal elements share the same position in both lists.
allNonEqualProp :: [Int] -> [Int] -> Bool
allNonEqualProp [] [] = True
allNonEqualProp (x:xs) (y:ys) | x == y = False
                    | otherwise = allNonEqualProp xs ys

-- Check if two lists are not equal.
inputOutputProp :: [Int] -> [Int] -> Bool
inputOutputProp xs ys = xs /= ys

-- Test isDerangement with sameLengthProp, allElementsProp, allNonEqualProp and inputOutputProp, 5 tests for each.
-- It may take a little time to run all tests.
testIsDerangement :: IO ()
testIsDerangement = do
                     result1 <- testPropDerans 1 5 isDerangement sameLengthProp
                     if (result1) then print ("sameLengthProp passed (5 tests)") else print ("sameLengthProp failed")
                     result2 <- testPropDerans 1 5 isDerangement allElementsProp
                     if (result2) then print ("allElementsProp passed (5 tests)") else print ("allElementsProp failed")
                     result3 <- testPropDerans 1 5 isDerangement allNonEqualProp
                     if (result3) then print ("allNonEqualProp passed (5 tests)") else print ("allNonEqualProp failed")
                     result4 <- testPropDerans 1 5 isDerangement inputOutputProp
                     if (result4) then print ("inputOutputProp passed (5 tests)") else print ("inputOutputProp failed")

-- In the following section we tried applying the same method from Exercise 3 to figure out which of the relational
-- properties used for testing the permutations and derangements functions is strongest.
-- The result however is disappointing, since it just returns the same list of properties as defined in strengthList'.
-- We hypothesize this may be due to the small amount of test cases in the domain, but we are not sure of this.
data RelProp a = RelProp ([a] -> [a] -> Bool) Int 
instance Show (RelProp a) where
    show (RelProp f n) = show n

prop5, prop6, prop7, prop8 :: RelProp Int
prop5 = RelProp sameLengthProp 5
prop6 = RelProp allElementsProp 6
prop7 = RelProp allNonEqualProp 7
prop8 = RelProp inputOutputProp 8

domain :: [([Int],[Int])]
domain = [([0,1,2,3,4],[3,2,1,4,0]),([0,1,2],[2,0,1])]

stronger' :: [([a],[a])] -> ([a] -> [a] -> Bool) -> ([a] -> [a] -> Bool) -> Bool
stronger' xs p q = forall xs (\(a,b) -> p a b --> q a b)  

strengthList' :: [RelProp Int]
strengthList' = sortBy ordFunc' [prop5, prop6, prop7, prop8]

ordFunc' :: RelProp Int -> RelProp Int -> Ordering
ordFunc' (RelProp f n) (RelProp f2 n2) | stronger' domain f f2 && stronger' domain f2 f = EQ
                                       | stronger' domain f f2 = LT
                                       | otherwise = GT

-- Exercise 6
-- Specification: for all letters in a string, they should be replaced by the 13th letter after it in the alphabet,
-- wrapping around back to 'a' after 'z'. Applying the rot13 algorithm to the result of the rot13 algorithm should
-- result in the original input string.

rot13 :: [Char] -> [Char]
rot13 line = map f line
    where f c | (ord c) + 13 > 122 = toEnum $ 96 + (13 - (122 - (ord c)))
              | otherwise = toEnum $ (ord c) + 13

-- Compares both arguments to see if the rot13 algorithm specification holds on the second one
-- with respect to the first one. It walks both lists recursively and compares its character values.
rot13Prop :: [Char] -> [Char] -> Bool
rot13Prop [] [] = True
rot13Prop (x:xs) (y:ys) | checkValues x y = rot13Prop xs ys
                        | otherwise = False
    where checkValues c1 c2 | ord c1 <= 109 = ord c1 + 13 == ord c2
          checkValues c1 c2 | ord c1 > 109  = ord c2      == 96 + (13 - (122 - ord c1))

-- Checks that when rot13 is applied to a string twice it is the same as the original string 
rot13TwiceProp :: [Char] -> Bool
rot13TwiceProp s = rot13 (rot13 s) == s

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
-- Then applies the rot13 algorithm to it and checks the altered string with rot13Prop to
-- see if it satisfies the postconditions of rot13.
testrot13Prop :: Int -> Int -> IO Bool
testrot13Prop k n = 
              if k == n 
              then return True
              else do
               i <- getRandomInt 100
               rnd <- getRandomCharList i
               rot <- return $ rot13 rnd
               if rot13Prop rnd rot && rot13TwiceProp rnd
                 then do testrot13Prop (k+1) n
                 else return False

-- The same as the previous method, but using rot13TwiceProp to check.
testrot13TwiceProp :: Int -> Int -> IO Bool
testrot13TwiceProp k n = 
              if k == n 
              then return True
              else do
                i <- getRandomInt 100
                rnd <- getRandomCharList i
                rot <- return $ rot13 rnd
                if rot13TwiceProp rnd
                then do testrot13TwiceProp (k+1) n
                else return False

testRot13 :: IO ()
testRot13 = do 
             result1 <- testrot13Prop 1 100
             if (result1) then print "rot13Prop passed (100 tests)" else print "rot13Prop failed"
             result2 <- testrot13TwiceProp 1 100
             if (result2) then print "rot13TwiceProp passed (100 tests)" else print "rot13TwiceProp failed"

-- Exercise 7

-- The only way to test this function is by generating invalid inputs. To do this you should carefully look at the IBAN algorithm
-- and try to find useful mathematical facts to construct a generator which generates invalid numbers. For now, here is a list of 
-- known valid and known invalid numbers.

-- List of (75) valid IBANs from https://www.iban.com/structure, 3 invalid IBANs and 2 non-IBANs
ibanList :: [String]
ibanList = ["AL35202111090000000001234567","AD1400080001001234567890","AT483200000012345864","AZ96AZEJ00000000001234567890","BH02CITI00001077181611",
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
         "AE460090000000123456789","GB98MIDL07009312345678","VG21PACG0000000123456789", "BR1512340000000010932840814P2", "MD21EX000012300001234567", "VG21PACG0000000123456788", "123", "Thisisnotanib@n"]

iban :: String -> Bool
iban (i:j:k:l:m) = fromDigits singles `mod` 97 == 1
    where replaced = map (toInteger . replaceChars) $ m ++ [i,j,k,l]
          singles = toSingles replaced
          replaceChars c | ord c >= 65 && ord c <= 90 = ord c - 55
                         | ord c >= 97 && ord c <= 122 = ord c - 87
                         | otherwise = ord c - 48
iban _ = False

-- Converts a list of integers into a single integer
-- e.g. [1,2,3,4] => 1234
fromDigits :: [Integer] -> Integer
fromDigits xs = aux xs 0
    where aux [] acc = acc
          aux (x:xs) acc  = aux xs ((acc * 10) + x)

-- Converts a list of integers into a list of single-digit numbers
-- e.g. [1,23,4] => [1,2,3,4]
toSingles :: [Integer] -> [Integer]
toSingles xs = f xs []
    where f [] ys = ys
          f (x:xs) ys | x >= 10 = f xs (ys ++ [x `div` 10, x `mod` 10])
                      | otherwise = f xs (ys ++ [x])

testIbanList :: Int -> Int -> [String] -> IO ()
testIbanList n failed []     = do
                                print $ show n ++ " tests finished, " ++ show failed ++ " failed"
testIbanList n failed (x:xs) = do
                                if (iban x)
                                then do testIbanList n failed xs
                                else do 
                                    print $ "Iban test failed for " ++ x
                                    testIbanList n (failed+1) xs

-- Result: 77 tests finished, 2 failed
testIban :: IO ()
testIban = testIbanList (length ibanList) 0 ibanList