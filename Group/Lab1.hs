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

-- Exercise 1

workshop2Test :: Int -> Bool
workshop2Test n = n > 0 --> squareSumList [1..n] == (n * (n + 1)*(2 * n + 1)) `div` 6
                      where squareSumList x = sum (map (^2) x)

workshop3Test :: Int -> Bool 
workshop3Test n = n > 0 --> cubeSumList [1..n] == (n * (n + 1) `div` 2) ^ 2
                      where cubeSumList x = sum (map (^3) x)

-- Exercise 2

workshop4Test :: Int -> Bool
workshop4Test n = n > 0 --> length (subsequences [1..n]) == 2 ^ n

-- The property is easy to test with regard to implementation,
-- however in complexity it becomes difficult. Large numbers for n
-- result in exponential sets, therefore the tests take a long time to
-- complete. The test isn't testing the mathematical property strictly
-- either. It really is just testing the properties of the subsequences
-- function.

-- Exercise 3

workshop5Test :: Int -> Bool
workshop5Test n = n > 0 --> length (permutations [1..n]) == fac n
    where fac = product . flip take [1..]

-- THe difficulty of testing is identical to the previous exercise. 

-- Exercise 4

reversablePrimes :: [Integer]
reversablePrimes = filter (not . prime . reversal) (takeWhile (<10000) primes)

-- The only way to test this function would be using the implementation of the function itself,
-- so testing it would prove rather difficult.

-- Exercise 5

first101Prime :: Integer
first101Prime = primes101 primes

primes101 :: [Integer] -> Integer
primes101 (p:ps) | prime sum101 = sum101
                 | otherwise    = primes101 ps
    where sum101 = sum (take 101 (p:ps))

-- You can never assume your function is correct without testing or proving it, but to
-- test this function you would again have to use its implementation, so only proving it is an option.

-- Exercise 6

refuteConjecture :: Integer
refuteConjecture = refute primes 1

refute :: [Integer] -> Int -> Integer
refute ps n | check ps n = refute ps (n + 1)
            | otherwise  = product (take n primes) + 1
    where check ps n = prime (product (take n primes) + 1)

-- Exercise 7

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
            helper (x:xs) flag acc | flag      = helper xs False (acc ++ [x * 2])
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

-- Exercise 8

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
honest = filter f boys
    where f x | accuses (head guilty) x = True
              | otherwise = False