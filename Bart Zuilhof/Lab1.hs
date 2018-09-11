
module Lab1 where
import Data.List
import Test.QuickCheck    

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

ex1f1 :: Integer -> Integer
ex1f1 n = sum $ map pow [1..n] 
  where pow x = x * x

test1 :: Integer -> Bool
test1 x = x > 0 --> ex1f1 x == ex1f2 x
  where ex1f2 x = (x * (x + 1) * (2 * x + 1)) `div` 6

ex2f1 :: Integer -> Integer
ex2f1 n = sum $ map (^3) [1..n] 
-- 45min

test2 :: Integer -> Bool
test2 x = x > 0 --> ex2f1 x == ex2f2 x
  where ex2f2 x = ((x * (x + 1)) `div` 2) ^ 2
-- 10min
ex5 :: Integer -> Int
ex5 n = length $ subsequences [1..n]

test3 :: Integer -> Bool
test3 x = (x > 0 && x < 100) --> ex5 x == 2 ^ x
--10min

-- The property is not hard to test. What we are actually testing
-- the function of subsequences, not the mathematical function.
-- so the question is if we are actually testing the property.

-- Exercise 3
factorial n = foldr (*) 1 [1..n]

test4 :: Integer -> Bool
test4 n = n > 0 && n < 5--> factorial n == toInteger(length(permutations [1..n]))  
-- 15min
-- quickJoren :: (Integer -> Bool) -> Bool
-- quickJoren f = foldr

-- Exercise 4
findPrimes = filter reverseablePrime primelist 
      where primelist = (takeWhile (<10000) primes)
            reverseablePrime x = prime (reversal x)
--15min
-- You could check for each element returned by the function if its 
-- reversable is a prime. But then you would write a function to test
-- to test itself. So that makes it rather difficult

-- Execise 5
findSumPrime :: [Integer] -> Integer
findSumPrime (fPrime : primes)  | prime currentIteration = currentIteration
                                | otherwise = findSumPrime primes
  where currentIteration = sum (take 101 (fPrime : primes))
--15min
-- Its always nice to test if you solution is correct. 
-- To test this you would have to write a similar function,
-- but there is no way of knowing if that function is correct.
-- So it is really hard to test.

-- Exercise 6
refuteConjecture :: [Integer] -> Int -> Integer
refuteConjecture primeList n | prime possiblePrime = refuteConjecture primeList (n + 1)
                             | otherwise = possiblePrime
  where possiblePrime = product (take n primeList) + 1
-- first counterexample that is returned is : 30031

-- Exercise 7
luhn :: Integer -> Bool
luhn ccNumber = sum (map (sum . intToList) (doubleDigits ((reverse . intToList) ccNumber) [] False)) * 9 `mod` 10 == 0

doubleDigits [] ccListAcc _ = ccListAcc
doubleDigits (c:cList) ccListAcc bit | bit = doubleDigits cList (ccListAcc ++ [c*2]) False
                                  | otherwise = doubleDigits cList (ccListAcc ++ [c]) True

intToList 0 = []
intToList i = intToList(i `div` 10) ++ [i `mod` 10]  
-- 20min
isAmericanExpress:: Integer -> Bool
isAmericanExpress cc = head (intToList cc) == 3 && luhn cc

isMaster :: Integer -> Bool
isMaster cc = head (intToList cc) == 5 && luhn cc

isVisa :: Integer -> Bool
isVisa cc = head (intToList cc) == 4  && luhn cc
-- 2min

-- Creating numbers on which the test has to fail is hard.
-- We could either pick 'incorrect' creditcard numbers from the internet.
-- But its not proven by us that they are correct.
-- We would have to modify the recipe so we would always end up
-- with incorrect luhn numbers.

incorrectLuhn = [2485745154367698, 3929830864649215, 1556984460879658, 6716070034226332, 9024007116793935]
correctLuhn = [4485745154367698, 4929830864649215, 4556984460879658, 4716070034226332, 4024007116793935]
testLuhn :: Bool
testLuhn = all luhn correctLuhn && not (any luhn incorrectLuhn)

accuses :: Boy -> Boy -> Bool
accuses Peter Jack = True
accuses Peter Matthew = True
accuses Carl Arnold = True
accuses _ _ = True

-- accusers :: Boy -> [Boy]

guilty, honest:: [Boy]
guilty = filter threeCorrectStatements boys
honest = filter (statement (head guilty)) boys

threeCorrectStatements :: Boy -> Bool
threeCorrectStatements boy = length (filter (==True) (map (statement boy) boys)) == 3

statement :: Boy -> Boy -> Bool
statement guilty Matthew = not (guilty == Carl) && not (guilty == Matthew) 
statement guilty Peter = (guilty == Matthew) || (guilty == Jack)
statement guilty Jack = (not (statement guilty Matthew)) && (not (statement guilty Peter))
statement guilty Arnold = statement guilty Matthew /= statement guilty Peter
statement guilty Carl = not (statement guilty Arnold)


-- getStatements = map statement [boys]


