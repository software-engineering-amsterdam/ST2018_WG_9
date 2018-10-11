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

# Exercise 4
### What is the least composite number that you can find that fools the check, for prime_tests_F k with k=1,2,3 ?
``` haskell
*Lecture6> testFermatPrimality 1 0 
9
*Lecture6> testFermatPrimality 2 0
9
*Lecture6> testFermatPrimality 3 0
91
```
## What happens if you increase k? 
The tests gets more accurate as you increase k
``` haskell 
fmap (\_-> randomRIO (2,n-1)) [1..k]`
```
where `k` dictates the number of samples of the infinite list
