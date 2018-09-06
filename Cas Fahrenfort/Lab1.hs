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

workshop2 :: Integer -> Bool
workshop2 n = n > 0 --> list == func
    where list = sum (map (^ 2) [1..n])
          func = (n * (n + 1) * (2 * n + 1)) `div` 6

-- 45 mins

workshop3 :: Integer -> Bool
workshop3 n = n > 0 --> list == func
    where list = sum (map (^ 3) [1..n])
          func = (^ 2) ((n * (n + 1)) `div` 2)

-- 5 mins

workshop4 :: Integer -> Bool
workshop4 n = n > 0 && n < 10 --> (2 ^ n) == length (subsequences [1..n])

-- 10 mins
-- The property is hard to test because the lists grow in size rapidly making the tests very lengthy to run.
-- Really you are testing the implementations of the functions length and subsequence, by using the mathematical
-- properties of cardinality and super sets.

workshop5 :: Integer -> Bool
workshop5 n = n > 0 && n < 5 --> foldr (*) 1 [1..n] == toInteger (length (permutations [1..n]))

-- 15 mins
-- The property is again hard to test because of the sizes of the lists involved
-- Again we are really testing the implementations of the permutations function, by using the mathematical
-- properties of factorial and set sizes.

reversablePrimes :: [Integer]
reversablePrimes = filter reversable primeList
    where reversable x = elem (reversal x) primeList
          primeList = takeWhile (< 10000) primes

-- 5 mins
