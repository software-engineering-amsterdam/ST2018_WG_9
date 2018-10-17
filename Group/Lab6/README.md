# Lab 6 - Group Submission

## Exercise 1

``` haskell
-- Call to a second function with an extra parameter
exM :: Integer -> Integer -> Integer -> Integer
exM b e m = exM' b (toBinary e) m (b `mod` m) 

-- Convert the exponent to a reverse binary number, then walk recursively through this list.
-- If the head of the list is a 1 then we multiply the new found result with our recursive call.
-- If we find a 0, then we skip this one but give them new found result to the recursive call

exM' :: Integer -> [Integer] -> Integer -> Integer -> Integer
exM' _ [] _ _  = 1
exM' b (x:xs) modulo pow | x == 1 = multM pow (exM' b xs modulo newMult) modulo
                         | otherwise = exM' b xs modulo newMult
        where newMult = multM pow pow modulo

-- Copied and modified from 'https://stackoverflow.com/questions/9166148/how-to-implement-decimal-to-binary-function-in-haskell'  
toBinary :: Integer -> [ Integer ]
toBinary 0 = []
toBinary n = (n `rem` 2) : toBinary(n `quot` 2 )
```

## Exercise 2

In ghci, `:set +s` Displays some stats after evaluating each expression, including the elapsed time and number of bytes allocated.

For the fast exponentiation, we are interested in the elapsed time of the function. For a set of calls for both functions, we can present a table with

* `*Lecture6> expM 7 256 13` results in `9` with `(0.01 secs, 73,016 bytes)`.   
  `*Lecture6> exM  7 256 13` results in `9` with `(0.00 secs, 76,728 bytes)`.  
* `*Lecture6> expM 8 35000632 9` results in `1` with `(0.87 secs, 39,115,040 bytes)`.  
  `*Lecture6> exM 8 35000632 9`  results in `1` with `(0.00 secs, 86,096 bytes)`.  
* `*Lecture6> expM 8 252525252525 9` results in `8` with `GNU MP: Cannot allocate memory (size=805474320)`.   
  `*Lecture6> exM 8 252525252525 9` results in `8` with `(0.01 secs, 91,432 bytes)`.  
* `*Lecture6> expM 2 829492394920 3` results in `GNU MP: Cannot allocate memory (size=4294950944)`.   
  `*Lecture6> exM 2 829492394920 3` results in `1` with `(0.00 secs, 543,448 bytes)`.  
* `*Lecture6> expM 7 87923469 13` results in `8` with `(2.73 secs, 87,932,024 bytes)`.  
  `*Lecture6> exM 7 87923469 13` results in `8` with `(0.00 secs, 87,808 bytes)`.  

As we can see, the `exM` implementation is consistently faster than the `expM` implementation. Additionally, in the case of large exponents, the bytes allocated are consistently lower for `exM`.

## Exercise 3

``` haskell
-- filter all non-primes from an infinte list starting at two
composites :: [Integer]
composites = filter (not . prime) [2..]
```

## Exercise 4, 5 and 6

These exercises have been combined together, since the implementations are similar and can thus be tested in a similar manner. For instance, if we consider the following function to fool Fermat Primality with composites and compare it to the one with Carmichael numbers, only one thing is changed. Same for carmichael and Miller-Robin. Here, `primeTestsF` is replaced by `primeMR`.
``` haskell
-- Composites
testFermatPrimality :: Int -> Integer -> IO Integer
testFermatPrimality k n = do bool <- primeTestsF k (composites !! fromIntegral n)
                             if not bool then testFermatPrimality k (fromIntegral n + 1)
                             else return (composites !! fromIntegral n)

-- Carmichaels
testFermatPrimality' :: Int -> Integer -> IO Integer
testFermatPrimality' k n = do bool <- primeTestsF k (carmichael !! fromIntegral n)
                              if not bool then testFermatPrimality' k (fromIntegral n + 1)
                              else return (carmichael !! fromIntegral n)

-- primeMR
testMillerRabin :: Int -> Integer -> IO Integer
testMillerRabin k n = do bool <- primeMR k (carmichael !! fromIntegral n)
                         if not bool then testMillerRabin k (fromIntegral n + 1)
                         else return (carmichael !! fromIntegral n)
```
We can write a function that finds the least composite number that fools the check by replicating function calls

``` haskell
infiniteTest :: (Monad m, Num t, Eq a) => (t1 -> t -> m a) -> t1 -> Int -> m [a]
infiniteTest f k n = do list <- replicateM n (f k 0)
                        let nubbed = nub list
                        return nubbed
```
Here, infiniteTest takes a monadic function `f`, an Int `k`, and an Int `n`, replicates that function `n` amount of times
with `k` accuracy in the test function. Finally, it returns a list without any duplicates. This function gives insight how the results of the primalityTests are distributed with changed `k` values.  

With this function, we can find interesting metrics such as the mean (if we remove `nub`) and variance (if we remove `nub`). However, we chose to use `nub` in order to find the least number that fools the check. Thus, function runners will appear as 

``` haskell
-- 100 replicates is empirically determined by us (trade-off between time and samples), k can be variably assigned in the minumum runner below
infiniteTestCompsites, infiniteTestCarmichael, infiniteTestMillerRabin:: Int -> IO [Integer]
infiniteTestCompsites   k = infiniteTest testFermatPrimality  k 100
infiniteTestCarmichael  k = infiniteTest testFermatPrimality' k 100
infiniteTestMillerRabin k = infiniteTest testMillerRabin      k 100

-- K = 1, change source code to change K 
infiniteTestCompositesMin, infiniteTestCarmichaelMin, infiniteTestMillerRabinMin:: IO Integer
infiniteTestCompositesMin  = do list <- infiniteTestCompsites 1                               
                                return $ minimum list
infiniteTestCarmichaelMin  = do list <- infiniteTestCarmichael 1
                                return $ minimum list
infiniteTestMillerRabinMin = do list <- infiniteTestMillerRabin 1
                                return $ minimum list
```                                
However, if we also pass the list of numbers as an argument, Exercise 4,5, and 6 can even be abbreviated further. However, there is some trade off in the verbosity and thus readability of the code. You'd have to keep track of an extra argument now to remove 1 function decleration. 

``` haskell
testFermatPrimality :: Int -> Integer -> [Integer] -> IO Integer
testFermatPrimality k n xs = do bool <- primeTestsF k (xs !! fromIntegral n)
                                if not bool then testFermatPrimality k (fromIntegral n + 1) xs
                                else return (xs !! fromIntegral n)

-- Using carmichael to test the Miller-Rabin primality check
testMillerRabin :: Int -> Integer -> [Integer] -> IO Integer
testMillerRabin k n xs = do bool <- primeMR k (xs !! fromIntegral n)
                            if not bool then testMillerRabin k (fromIntegral n + 1) xs
                            else return (xs !! fromIntegral n)

-- infiniteTest takes a monadic function, Int 'k', an Int 'n', and a list of numbers 'xs', replicates that function 'n' amount of times
-- with 'k' accuracy in the test function. Finally, it returns a list without any duplicates.
-- This function by itself gives insight how the results of the primalityTests are distributed with changed k values. 
infiniteTest :: (Monad m, Num t, Eq a) => (t2 -> t -> t1 -> m a) -> t2 -> Int -> t1 -> m [a]
infiniteTest f k n xs = do list <- replicateM n (f k 0 xs)
                           let nubbed = nub list
                           return nubbed

-- 100 replicates is empirically determined by us, k can be variably assigned in the minumum runner below
infiniteTestCompsites, infiniteTestCarmichael, infiniteTestMillerRabin:: Int -> IO [Integer]
infiniteTestCompsites k   = infiniteTest testFermatPrimality  k 100 composites
infiniteTestCarmichael k  = infiniteTest testFermatPrimality  k 100 carmichael
infiniteTestMillerRabin k = infiniteTest testMillerRabin      k 100 carmichael

```
Examples of infiniteTest with `k=1` and `n=100`

``` haskell
*Lecture6> infiniteTestCompsites 1
[55,9,35,21,25,65,105,15,85,45,217,91,28,39,165,33,115,49,63,27,70,133,76,247]  

*Lecture6> infiniteTestCarmichael 1
[294409,56052361]

*Lecture6> infiniteTestMillerRabin 1
[294409,118901521,56052361,13079177569,100264053529,228842209,2301745249,1299963601,3414146271409,21515221081,172018713961,1201586232601,527519713969,172947529,216821881,2724933935809,168003672409,1396066334401,65700513721,71171308081,9624742921,663805468801,11346205609,27278026129,173032371289,1797002211241]
```

### What is the least composite number that you can find that fools the check, for prime_tests_F k with k=1,2,3 ?
|    Method    |  K = 1 |   K = 2   |   K = 3   |      K = 4     |
|:------------:|:------:|:---------:|:---------:|:--------------:|
|  Composites  |    9   |     9     |     9     |       85       |
|  Carmichael  | 294409 |   294409  |   294409  |     294409     |
| Miller-Rabin | 294409 | 118901521 | 118901521 | 57060521336809 |

## What happens if you increase k? 
The tests gets more accurate as you increase k
``` haskell 
fmap (\_-> randomRIO (2,n-1)) [1..k]`
```
where `k` dictates the number of samples of the infinite list  
Any number can return a false positive, when we increase `k` we run more tests so the chance of returning multiple false positives goes down, and the test becomes more accurate.

### Findings when using the Fermats primality check on the Carmicheal numbers
The Carmichael test is likely to return one of the first elements of the Carmichael numbers. This is because all the Carmichael numbers are by definition composites that satisfy the fermat property (if the base number is not divisible by the exponent).

### Findings when using the Miller-Rabin primality check on the Carmicheal numbers
The numbers returned are much larger, this would mean that the MR primality test is less accurate than the Fermat primality test. Empirically, it is also found that this test is slower than Fermat's test.

## Exercise 7
### Finding Mersenne primes
``` haskell

-- Runner for Mersenne function
testMarsennePrimesRunner :: Int -> IO ()
testMarsennePrimesRunner k = discoverMarsennePrimes 0 k primes
```
Checks for the nth prime and higher if it is a Mersenne prime, printing the progress.  
First argument `c` is the counter  
Second argument `k` is the k used in Miller-Rabin primality check  
Third argument `list` is the list of primes  

``` haskell
discoverMarsennePrimes :: Int -> Int -> [Integer] -> IO ()
discoverMarsennePrimes _ _ [] = print "Empty List"
discoverMarsennePrimes c k (x:xs) = do bool <- primeMR k (2^x - 1)
                                       if bool
                                       then do print ("#" ++ show c ++ " Marsenne number with (2^" ++ show x ++ ")-1 ") -- number: " ++ show (2^x - 1))
                                               discoverMarsennePrimes (c+1) k xs
                                       else discoverMarsennePrimes c k xs
```
We ran it for 45 minutes with k = 1 and we found 24 Mersenne primes.
``` haskell 
*Lecture6> testMarsennePrimesRunner 1
"#0 Marsenne number with (2^2)-1 "
"#1 Marsenne number with (2^3)-1 "
"#2 Marsenne number with (2^5)-1 "
"#3 Marsenne number with (2^7)-1 "
"#4 Marsenne number with (2^13)-1 "
"#5 Marsenne number with (2^17)-1 "
"#6 Marsenne number with (2^19)-1 "
"#7 Marsenne number with (2^31)-1 "
"#8 Marsenne number with (2^61)-1 "
"#9 Marsenne number with (2^89)-1 "
"#10 Marsenne number with (2^107)-1 "
"#11 Marsenne number with (2^127)-1 "
"#12 Marsenne number with (2^521)-1 "
"#13 Marsenne number with (2^607)-1 "
"#14 Marsenne number with (2^1279)-1 "
"#15 Marsenne number with (2^2203)-1 "
"#16 Marsenne number with (2^2281)-1 "
-- ... etc
```
This function does return genuine Mersenne primes as found online (https://www.mersenne.org/primes).

