# Exercise 1

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

# Exercise 2

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

# Exercise 3

``` haskell
-- filter all non-primes from an infinte list starting at two
composites :: [Integer]
composites = filter (not . prime) [2..]
```

# Exercise 4
### What is the least composite number that you can find that fools the check, for prime_tests_F k with k=1,2,3 ?


``` haskell
compositeFTest :: IO Integer
compositeFTest = test composites
    where test (x:xs) = do a <-primeTestsF 3 x
                           if a then
                            return x
                           else
                            test xs

-- For k=1, the test returns 21
-- For k=2, the test returns 341
-- For k=3, the test returns 1541
-- When k increases, the chance for a false positive decreases because of the all check
-- In the test.
```

## What happens if you increase k? 
The tests gets more accurate as you increase k
``` haskell 
fmap (\_-> randomRIO (2,n-1)) [1..k]`
```
where `k` dictates the number of samples of the infinite list


# Exercise 5
### Findings when using the Fermats primality check on the Carmicheal numbers

``` haskell
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

-- The Carmichael test is likely to return one of the first elements of the 
-- Charmichaels numbers. This is because all the Carmichael numbers are by definition
-- composites that satisfy the fermat property (if the base number is not divisible by the
-- exponent).
```

# Exercise 6
### Findings when using the Robin-Miller primality check on the carmicheal numbers
``` haskell
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
```

# Exercise 7
### Findings when using the Robin-Miller primality check on the carmicheal numbers
``` haskell
mersenneTest :: IO [Integer]
mersenneTest n = do
                let p = primes !! n
                let m = 2^p -1
                b <- primeMR 2 m 
                if b then do
                    print p
                    mersenneTest (n+1)
                else 
                    mersenneTest (n+1)
```