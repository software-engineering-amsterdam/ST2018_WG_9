module Lab1 where

import Data.List
import Test.QuickCheck

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

nextPrime :: Integer -> Integer
nextPrime n = if prime n then n else nextPrime (n+1)

-- Generates an infinite list of primes
primes :: [Integer]
primes = 2 : filter prime [3..]

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

data Boy = Matthew | Peter | Jack | Arnold | Carl
    deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-- | Exercise 1 (Ex. 2 and Ex. 3 from Workshop 1) | Time: 15min + 7min = 22min
exercise2Test :: Int -> Bool
exercise2Test n = n > 0 --> squareSumList [1..n] == (n * (n + 1)*(2 * n + 1)) `div` 6
                      where squareSumList x = sum (map (^2) x)

exercise3Test :: Int -> Bool 
exercise3Test n = n > 0 --> cubeSumList [1..n] == (n * (n + 1) `div` 2) ^ 2
                      where cubeSumList x = sum (map (^3) x)

-- | Exercise 2 (Ex. 4 from Workshop 1) | Time: 15min
exercise4Test :: Int -> Bool
exercise4Test n = n > 0 && n < 6 --> length (subsequences [1..n]) == 2 ^ n

-- | Exercise 3 (Ex. 5 from Workshop 1) | Time: 25min
exercise5Test :: Int -> Bool
exercise5Test n = n > 0 && n < 6 --> length (permutations [1..n]) == fac n
                      where fac = product . flip take [1..]

-- | Exercise 4 | Time: 25min, mainly because I wanted to make it look nice
findReversablePrimes :: [Integer]
findReversablePrimes = filter (not . prime . reversal) (takeWhile (<10000) primes)

-- I would test this function by...

-- | Exercise 5 | Time: 15min
smallestPrimeRunner :: Integer
smallestPrimeRunner = smallestPrime primes

smallestPrime :: [Integer] -> Integer
smallestPrime xs | prime (sumOfConescutives xs) = sumOfConescutives xs
                 | otherwise                    = smallestPrime $ drop 1 xs
                 where 
                     sumOfConescutives xs = sum (take 101 xs)

-- Do you have to test that your answer is correct? How could this be checked?

-- | Exercise 6 | Time: 30 min
refuteConjectureRunner :: Integer
refuteConjectureRunner = refuteConjecture 1

refuteConjecture :: Integer -> Integer
refuteConjecture n | prime (productOfConsecutives n) = refuteConjecture $ n + 1
                   | otherwise                       = productOfConsecutives n
                   where
                       productOfConsecutives n' = product (take' n' primes) + 1
                       take' n                  = take (fromIntegral n)


-- | Exercise 7 | Time: 60 min, due to edge cases with numbers of even and uneven length

luhnTest :: Bool
luhnTest = isVisa 4485396539746600

luhn :: Integer -> Bool
luhn x = sum (map sumDigits (mapOdd (*2) (reverse (digits x)))) * 9 `mod` 10 == 0

digits :: Integer -> [Int]
digits = map (read . (:[])) . show

sumDigits :: Int -> Int
sumDigits 0 = 0
sumDigits x = (x `mod` 10) + sumDigits (x `div` 10)

mapEven :: (a->a) -> [a] -> [a]
mapEven f [] = []
mapEven f (x:xs) = f x : mapOdd f xs

mapOdd :: (a->a) -> [a] -> [a]
mapOdd g [] = []
mapOdd g (x:xs) = x : mapEven g xs

isAmericanExpress :: Integer -> Bool
isAmericanExpress x = head (digits x) == 3 && luhn x

isMaster :: Integer -> Bool
isMaster x = head (digits x) == 5 && luhn x

isVisa :: Integer -> Bool
isVisa x = head (digits x) == 4 && luhn x