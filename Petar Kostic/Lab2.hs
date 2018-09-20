module Lab2 where
 
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Lecture2

-- ################################### Exercise 1 ###################################
-- Time: 45 min

-- Red Curry claims that the numbers returned by probs are random in the open interval (0..1).
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
          p <- getStdRandom random
          ps <- probs (n-1)
          return (p:ps)

-- In order to check this, we assign a number for each number in the list and add this to an accumulator
probsCheck :: [Float] -> [Int]
probsCheck []     = []
probsCheck (x:xs) | x < 0.25  = 1 : probsCheck xs
                  | x < 0.50  = 2 : probsCheck xs
                  | x < 0.75  = 3 : probsCheck xs
                  | otherwise = 4 : probsCheck xs

-- Given the number of ranges and the list of probabilities, construct a list that counts of length n (1st argument) with the amount of
-- occurrences of probsCheck's results
probsCheckCounter :: Int -> [Float] -> [Int]
probsCheckCounter _ []     = []
probsCheckCounter 0 _      = []
probsCheckCounter n xs = count n (probsCheck xs) : probsCheckCounter (n-1) xs
                           where count x = length . filter (x==)

-- Note that, due to the nature of probsCheckCounter, the resulting list will be for quartiles 4 to 1
probsRunner :: IO [Int]
probsRunner = do
              xs <- probs 100000
              putStr "Frequencies for the following order: [0.75..1),[0.5..0.75),[0.25..0.5),(0..0.25) \n"
              return (probsCheckCounter 4 xs)
--       Test results:  
--       Frequencies for the following order: [0.75..1),[0.5..0.75),[0.25..0.5),(0..0.25)
--       [25024,25095,25091,24790]

-- ################################### Exercise 2 ###################################
-- Time: 30 min
data Shape = NoTriangle | Equilateral 
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

-- Write a program (in Haskell) that takes a triple of integer values as arguments and gives as output one of the following statements:
--     Not a triangle (Geen driehoek) if the three numbers cannot occur as the lengths of the sides of triangle,
--     Equilateral (Gelijkzijdig) if the three numbers are the lengths of the sides of an equilateral triangle,
--     Rectangular (Rechthoekig) if the three numbers are the lengths of the sides of a rectangular triangle,
--     Isosceles (Gelijkbenig) if the three numbers are the lengths of the sides of an isosceles (but not equilateral) triangle,
--     Other (Anders) if the three numbers are the lengths of the sides of a triangle that is not equilateral, not rectangular, and not isosceles.

isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c = a > 0 && b > 0 && c > 0 && (a + b > c) && (a + c > b) && (b + c > a)

-- An Equilateral triangle has all sides the same length.
isEquilateralTriangle :: Int -> Int -> Int -> Bool
isEquilateralTriangle a b c = a == b && b == c

-- A Rectangular triangle has at least one permutation where pythagoras holds
isRectangularTriangle :: Int -> Int -> Int -> Bool
isRectangularTriangle a b c = a*a + b*b == c*c || a*a + c*c == b*b || b*b + c*c == a*a

-- An Isosceles triangle with at least two sides of the same size. This is safe due to the order of pattern matching in triangles
isIsoscelesTriangle :: Int -> Int -> Int -> Bool
isIsoscelesTriangle a b c = a == b || a == c || b == c

-- Implement all 4 types of triangle checkers, and assign the correct datatype for a lengths a, b, c
triangle :: Int -> Int -> Int -> Shape
triangle a b c | not (isTriangle a b c)      = NoTriangle
               | isRectangularTriangle a b c = Rectangular
               | isEquilateralTriangle a b c = Equilateral
               | isIsoscelesTriangle   a b c = Isosceles
               | otherwise                   = Other

-- ################################### Exercise 3 ################################### 
-- Time: 45 min, mostly wasted on creating show instances for functions, but these cannot be pattern matched...

-- Stealing the type notation of stronger, weaker :) didn't know that was possible!
prop1, prop2, prop3, prop4 :: Int -> Bool
prop1   = even
prop2 x = even x && x > 3                   -- or even 
prop3 x = even x || x > 3                   -- or even
prop4 x = (even x && x > 3) || even x       -- or even

-- Creating show instances for functions of Int -> Bool is not possible (trust me i tried), so a different solution must do
propList :: [(Int -> Bool, String)]
propList = [(prop1, "even")
           ,(prop2, "(even x && x > 3")
           ,(prop3, "(even x || x > 3")
           ,(prop4, "((even x && x > 3) || even x)")]

-- Having to import Data.List already implied that our owh GT, LT (and EQ?) 
-- must be implemented for the stronger function in order to sort the strongest.
-- These definition have been made for descending order
strongerOrd :: [a] -> (a -> Bool) -> (a -> Bool) -> Ordering
strongerOrd xs p q | stronger xs p q && stronger xs q p = EQ
                   | stronger xs p q                    = LT
                   | stronger xs q p                    = GT 

-- Now to use sortBy that implements this orderer, only uses the first element of the tuples, and shows the second
strongOrderer :: [String]
strongOrderer = map snd (sortBy f propList)
                    where f x y = strongerOrd [-10..10] (fst x) (fst y)
-- *Lab2> strongOrderer
-- ["(even x && x > 3","even","((even x && x > 3) || even x)","(even x || x > 3"]

-- ################################### Exercise 4 ###################################  - Time: 10 min
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = all (`elem` xs) ys && (xs /= ys) && (length xs == length ys)

-- ################################### Exercise 5 ###################################  - Time: 30 min
-- isExclusive checks whether an iterative index occurence of the heads of the list are not identical
isExclusiveIndex :: [Int] -> [Int] -> Bool
isExclusiveIndex []      []    = True
isExclusiveIndex (x:xs) (y:ys) = x /= y && isExclusiveIndex xs ys

-- isDerangement checks whether the whole lists are permutations of eachother before checking the exclusivity of the indices
isDerangement :: [Int] -> [Int] -> Bool
isDerangement xs ys = isPermutation xs ys && isExclusiveIndex xs ys

-- deran generates a list of all derangements of the list [0..n-1].
deran :: Int -> [[Int]]
deran n = filter (isDerangement xs) (permutations xs)
        where xs = [0..n-1]

-- ################################### Exercise 6 ###################################  
-- Time: 40 min, mostly spend on browsing Data.Char (don't use if often)

-- ['a'..'z'] does not work, 'm' can correctly map to 'z', but 'n' maps to a character outside of the a-z alphabet
-- That's why a backwards rotation is specified from this cutoff point. Note that special cases have to be made for
-- capital letters. If it is not a character affected by rot13, we keep it in place
rot13ByChar :: Char -> Char
rot13ByChar c | c `elem` (['a'..'m'] ++ ['A'..'M']) = c `charAdd` 13
              | c `elem` (['n'..'z'] ++ ['N'..'Z']) = c `charAdd` (-13)
              | otherwise                           = c
              where 
                charAdd c n = chr (ord c + n)

-- Now this function can be mapped on a string
rot13 :: String -> String
rot13 = map rot13ByChar

-- ################################### Exercise 7 ################################### 
-- Time: 90 min

-- Replace each letter in the string with two digits, thereby expanding the string,
-- Interpret the string as a decimal integer and compute the remainder of that number on division by 97
iban :: String -> Bool
iban xs = (concatListToOneNumber (charsToDigits (move4Initials xs)) `mod` 97) == 1 

-- Moves the four initial indices of a a list to the end of a list
move4Initials :: [a] -> [a]
move4Initials xs = drop 4 xs ++ take 4 xs

-- Replaces each letter by its two digit interpretation or throws an error. Ignore spaces before checking the rest
--  where A = 10, B = 11, ..., Z = 35
charsToDigits :: String -> [Int]
charsToDigits []     = []
charsToDigits (x:xs) | x == ' '                          = charsToDigits xs
                     | x `elem` ['0'..'9']               = digitToInt x         : charsToDigits xs 
                     | x `elem` ['a'..'z'] ++ ['A'..'Z'] = ord (toUpper x) - 55 : charsToDigits xs
                     | otherwise                         = error "Invalid Character in IBAN"

concatListToOneNumber :: [Int] -> Integer
concatListToOneNumber = read . concatMap show
-- To convert a list of digits to one number, doing it the following way converts [1,2,30] to 150, an undesired result.. 
-- concatListOfDigits :: [Int] -> Int
-- concatListOfDigits = foldl (\x y -> 10 * x + y) 0
-- That's why I default to the read . concatMap show method. Although ugly, 
-- since we convert back and forth, it has a perfect usecase here.

-- TODO: - throw false on invalid characters instead of an error
--       - check countries

-- List of valid IBANs from https://www.iban.com/structure
validIbans :: [String]
validIbans = ["AL35202111090000000001234567","AD1400080001001234567890","AT483200000012345864","AZ96AZEJ00000000001234567890","BH02CITI00001077181611",
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

validChecker :: Bool
validChecker = all iban validIbans
-- *Lab2> validChecker
-- True