
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

workshop2 :: Integer -> Integer
workshop2 n = sum(map pow [1..n])
    where pow x = x * x

workshop3 :: Integer -> Integer
workshop3 n = sum(map (^ 3) [1..n])

ex1_test1 :: Integer -> Bool -- 45 mins
ex1_test1 x = x > 0 --> workshop2 x == rhs x
  where rhs y = (y * (y + 1) * (2 * y + 1)) `div` 6

ex1_test2 :: Integer -> Bool -- 5 mins
ex1_test2 x = x > 0 --> workshop3 x == rhs x
  where rhs x = (x * (x + 1) `div` 2) ^ 2

workshop4 :: Integer -> Int
workshop4 n = length$subsequences [1..n]


-- 10 mins --
-- The property is easy to test with regard to implementation,
-- however in complexity it becomes difficult. Large numbers for n
-- result in exponential sets, therefore the tests take a long time to
-- complete. The test isn't testing the mathematical property strictly
-- either. It really is just testing the properties of the subsequences
-- function.
ex2_test1 :: Integer -> Bool
ex2_test1 n = n > 0 && n < 10 --> workshop4 n == 2 ^ n

-- 15 mins --
-- The test is again a test of the build in function implementation instead
-- of testing a mathetmatical fact. On top of that it's very shallow as it only
-- accepts numbers from 1 to 5.
ex3_test1 :: Integer -> Bool
ex3_test1 n = n > 0 && n < 5 --> toInteger(length(permutations [1..n])) == toInteger(foldr (*) 1 [1..n])


-- 15 mins --
-- To test function emperically would be rather difficult. The only thing
-- we can do is to check for each returned element if it is again a prime
-- and if it's reversable. This wouldn't test anything however, since that
-- is the exact implementation of the function.
allPrimesReversal :: [Integer]
allPrimesReversal = filter isrevPrime (takeWhile (< 10000) primes)
  where isrevPrime x = prime(reversal x)


-- 10 mins --
-- Mathematically the function holds, so testing is not necessary
ex5 :: Integer
ex5 = hprimes primes

hprimes :: [Integer] -> Integer
hprimes (p:rimes) = if prime(currentPrime) then currentPrime else hprimes rimes
  where currentPrime = sum(take 101 (p:rimes))

-- 10 mins --
-- 30031
ex6 :: Integer
ex6 = refuteConjecture primes 1

refuteConjecture :: [Integer] -> Int -> Integer
refuteConjecture (p:rimes) n  | prime(prodPrime n) || p < 2  = refuteConjecture (p:rimes) (n + 1)
                              | otherwise = prodPrime n
  where prodPrime n = product (take n primes) + 1

-- Exercise 7: 30 minutes
-- 79927398713 is correctly validated
luhn :: Integer -> Bool
luhn x =  sum(map sumDigit (doubleEvery decomp [] False)) * 9 `mod` 10 == 0
  where decomp = reverse (intToList x)

doubleEvery :: [Integer] -> [Integer] -> Bool -> [Integer]
doubleEvery [] acc flag = acc
doubleEvery (x:xs) acc flag | flag = doubleEvery xs (acc ++ [x*2]) False
                            | otherwise = doubleEvery xs (acc ++ [x]) True

sumDigit :: Integer -> Integer
sumDigit i = sum(intToList i)

intToList :: Integer -> [Integer]
intToList 0 = []
intToList i =  intToList(i `div` 10) ++ [(i `mod` 10)]

isAmericanExpress :: Integer -> Bool
isAmericanExpress x = head (intToList x) == 3 && luhn x

isMaster :: Integer -> Bool
isMaster x = head (intToList x) == 5 && luhn x

isVisa :: Integer -> Bool
isVisa x = head (intToList x) == 4 && luhn x

-- To test the lune algorithm one would have to look at the individual steps of the
-- algorithm and find mathematical properties that can be tested. To then find wrong
-- lune numbers, one of the steps of the algorithm could be altered so one of the
-- mathematical properties doesn't hold anymore. The following test is a bit more simple;
-- testing the luhn algorithm with numbers that are proven incorrect by external sources.

incorrectLunes :: [Integer]
incorrectLunes = [79927328210, 79427398711, 79927398732, 7992739824, 79327398714, 799273987234, 3229927398716, 723427398717, 79927398718, 79927398719]

correctLunes :: [Integer]
correctLunes = [341391823299731, 379972169675173, 376707429031764, 371329687026363, 378227443244390]

luhnTestIncorrect :: Bool
luhnTestIncorrect = all (== False) (map luhn incorrectLunes)

luhnTestCorrect :: Bool
luhnTestCorrect = all (== True) (map luhn correctLunes)

-- Exercise 8 --
accuses :: Boy -> Boy -> Bool
accuses Peter Jack = True
accuses Peter Matthew = True
accuses Matthew Carl = True
