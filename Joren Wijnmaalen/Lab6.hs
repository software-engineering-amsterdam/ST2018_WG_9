module Lab6 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture6

-- Exercise 1
-- Time: 1.5h

-- The exponent is mapped to a binary list
-- Every bit can be calculated with exMBinary, and the final solution is folded * mod 13
-- exMBetter :: Integer -> Integer -> Integer -> Integer
-- exMBetter a ex m = let l = map (\(c, d) -> if c == 1 then exMBinary a (2 ^ d) m else 1 ) (toBinary ex 0)
--                    in rem (product l) m


-- exMBinary :: Integer -> Integer -> Integer -> Integer
-- exMBinary a ex m = exMBinary' (rem a m) m 0 ex

-- exMBinary' :: Integer -> Integer -> Integer -> Integer -> Integer
-- exMBinary' prev m c n | (2^c) == n = prev
--                       | otherwise = exMBinary' (multM prev prev m) m (c + 1) n

-- toBinary :: Integer -> Integer -> [(Integer, Integer)]
-- toBinary 0 acc = [(0, acc)]
-- toBinary n acc = (n `rem` 2, acc) : toBinary (n `quot` 2) (acc + 1)

-- *Lecture6> exM 2 829492394920 3 results in 1 with (0.00 secs, 543,448 bytes)
-- *Lecture6> expM 2 829492394920 3 results in GNU MP: Cannot allocate memory (size=4294950944)

-- Exercise 2
-- SEE REPORT

-- Exercise 3

-- composites' :: [Integer]
-- composites' = composites'' [2..]
--     where composites'' (x:xs) | prime x = composites'' xs
--                               | otherwise = x : composites'' xs

-- Exercise 4
-- 30 mins
compositeFTest :: IO Integer
compositeFTest = test composites
    where test (x:xs) = do a <-primeTestsF 3 x
                           if a then
                            return x
                           else
                            test xs

-- For k=1, the test returns 21
-- For k=2, the test returns 1729
-- For k=3, the test returns 1541
-- When k increases, the chance for a false positive decreases because of the all check
-- In the test.


-- Exercise 5
-- 5 min
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      prime (6*k+1), 
      prime (12*k+1), 
      prime (18*k+1) ]

carmichaelFTest :: IO Integer
carmichaelFTest = test carmichael
    where test (x:xs) = do a <-primeTestsF 1 x
                           if a then
                            return x
                           else
                            test xs

-- The carmichael test is likely to return one of the first elements of the 
-- charmichaels numbers. This is because all the carmichael numbers are by definition
-- composites that satisfy the fermat property (if the base number is not divisible by the
-- exponent).

-- Exercise 6
carmichaelMRTest :: IO Integer
carmichaelMRTest = test carmichael
    where test (x:xs) = do a <-primeMR 1 x
                           if a then
                            return x
                           else
                            test xs
-- The numbers returned are much larger, this would mean that the MR primality test is less accurate
-- than the Fermat primality test. Emperically, it is also found that this test is slower
-- than Fermat's test.

-- Exercise 7
mersennePrimes :: IO [Integer]
mersennePrimes = let possiblePrimes = map (\x -> 2^x - 1) (take 20 primes)
                 in filterRealPrimes possiblePrimes
                
filterRealPrimes :: [Integer] -> IO [Integer]
filterRealPrimes [] = return []
filterRealPrimes (x:xs) = do a <- primeMR 1 x
                             if a then do
                              ls <- filterRealPrimes xs
                              return $ x : ls
                             else
                              filterRealPrimes xs