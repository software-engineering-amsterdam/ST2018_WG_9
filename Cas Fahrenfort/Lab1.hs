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

-- { Exercise 1 }
-- 45 mins

workshop2 :: Integer -> Bool
workshop2 n = n > 0 --> list == func
    where list = sum (map (^ 2) [1..n])
          func = (n * (n + 1) * (2 * n + 1)) `div` 6

workshop3 :: Integer -> Bool
workshop3 n = n > 0 --> list == func
    where list = sum (map (^ 3) [1..n])
          func = (^ 2) ((n * (n + 1)) `div` 2)

-- { Exercise 2}
-- 10 mins

workshop4 :: Integer -> Bool
workshop4 n = n > 0 && n < 10 --> (2 ^ n) == length (subsequences [1..n])

-- The property is hard to test because the lists grow in size rapidly making the tests very lengthy to run.
-- Really you are testing the implementations of the functions length and subsequence, by using the mathematical
-- properties of cardinality and super sets.

-- { Exercise 3 }
-- 15 mins

workshop5 :: Integer -> Bool
workshop5 n = n > 0 && n < 5 --> foldr (*) 1 [1..n] == toInteger (length (permutations [1..n]))

-- The property is again hard to test because of the sizes of the lists involved
-- Again we are really testing the implementations of the permutations function, by using the mathematical
-- properties of factorial and set sizes.

-- { Exercise 4 }
-- 5 mins

reversablePrimes :: [Integer]
reversablePrimes = filter reversable primeList
    where reversable x = elem (reversal x) primeList
          primeList = takeWhile (< 10000) primes

-- Testing this function would be difficult, mostly because what we just wrote in itself is already basically the test.
-- You could apply the function again to check if each prime is reversable, but we would use the implementation to test
-- the implementation.

-- { Exercise 5 }
-- 20 mins

primes101 :: [Integer] -> Integer
primes101 (p:ps) | prime sum101 = sum101
                 | otherwise = primes101 ps
    where sum101 = sum (take 101 (p:ps))

-- It is not smart to assume it is true without testing. However, I would not know how to test it.

-- { Exercise 6 }
-- 20 mins

refute :: [Integer] -> Int -> Integer
refute ps n | check ps n = refute ps (n + 1)
            | otherwise = product (take n primes) + 1
    where check ps n = prime (product (take n primes) + 1)

-- { Exercise 7 }
-- 50 mins

luhn :: Integer -> Bool
luhn cc = sum (summed doubled) * 9 `mod` 10 == 0
    where doubled = doubleSeconds $ (reverse . digits) cc
          summed xs = map (sum . digits) xs

-- Turn number into list of its digits
digits :: Integer -> [Integer]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

-- Double every second number using an accumulative variable
doubleSeconds :: [Integer] -> [Integer]
doubleSeconds is = helper is False []
    where   helper [] flag acc = acc
            helper (x:xs) flag acc | flag = helper xs False (acc ++ [x * 2])
                                   | otherwise = helper xs True (acc ++ [x])

isAmericanExpress :: Integer -> Bool
isAmericanExpress cc = head dig == 3 && length dig == 15 && luhn cc
    where dig = digits cc

isVisa :: Integer -> Bool
isVisa cc = head dig == 4 && length dig == 16 && luhn cc
    where dig = digits cc

isMasterCard :: Integer -> Bool
isMasterCard cc = head dig == 5 && length dig == 16 && luhn cc
    where dig = digits cc

-- To fully test this algorithm, you should go through the algorithm step by step and find
-- relevant mathematical facts, to create a function which generates invalid Luhn numbers.
-- For now, here is a test with a list of examples and counterexamples.

luhnExamples = [79927398713, 4916253459761748, 6011657420991490, 349791407048630]
luhnCounters = [1, 79927398710, 349791407048631]

luhnTest :: Bool
luhnTest = all luhn luhnExamples && not (any luhn luhnCounters)

-- { Exercise 8 }
-- 2 hours

accuses :: Boy -> Boy -> Bool
accuses guilty Matthew = guilty /= Matthew && guilty /= Carl
accuses guilty Peter = (guilty == Matthew) || (guilty == Jack)
accuses guilty Jack = not (accuses guilty Matthew) && not (accuses guilty Peter)
accuses guilty Arnold = (accuses guilty Matthew || accuses guilty Peter) && (not (accuses guilty Matthew) && not (accuses guilty Peter))
accuses guilty Carl = not (accuses guilty Arnold)

guilty :: [Boy]
guilty = filter f boys
    where f x | length ( filter (== True) (map (accuses x) boys)) == 3 = True
              | otherwise = False

honest :: [Boy]
honest = filter (accuses (head guilty)) boys
