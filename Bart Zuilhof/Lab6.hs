module Lab6

where 

import Lecture6


--  Exercise  1
-- Timespent: 1,5h

-- exM :: Integer -> Integer -> Integer -> Integer
-- exM b e m = exM' b (toBinary e) m (b `mod` m) 

-- exM' :: Integer -> [Integer] -> Integer -> Integer -> Integer
-- exM' _ [] _ _  = 1
-- exM' b (x:xs) modulo pow | x == 1 = multM pow (exM' b xs modulo newMult) modulo
--                          | otherwise = exM' b xs modulo newMult
--         where newMult = multM pow pow modulo
  
-- toBinary :: Integer -> [ Integer ]
-- toBinary 0 = []
-- toBinary n = (n `rem` 2) : toBinary(n `quot` 2 )

-- Exercise 2
-- Time spent: 5min
-- *Lab6> exM 7 87923469 13
-- 8
-- (0.00 secs, 87,808 bytes)

-- *Lab6> expM 7 87923469 13
-- 8
-- (2.73 secs, 87,932,024 bytes)

-- Exercise 3
-- Time spent: 5min 

-- composites :: [Integer]
-- composites = filter (not . prime) [2..]

-- Exercise 4
-- time spent: 10min
testFermCheck :: IO Integer
testFermCheck = testFermCheck' carmichael
testFermCheck' (x:xs) = do 
                    a <- primeTestsF 5 x 
                    if a then return x
                    else testFermCheck' xs
-- k=1 results into 45
-- k=2 results into 341
-- k=3 results into 6601
-- The further we are increase k, the chance of false negatives will decrease. So the numbers will keep increasing

-- Exercise 5
-- Time spent: 10min
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
       k <- [2..], 
       prime (6*k+1), 
       prime (12*k+1), 
       prime (18*k+1) ]

-- carlmicheal numbers are numbers that return true in the permats primality test but are not actual prime numbers

-- Exercise 6
-- time spent: 10min
testMRCheck :: IO Integer
testMRCheck = testFermCheck' carmichael
testMRCheck' (x:xs) = do 
                    a <- primeMR 5 x 
                    if a then return x
                    else testMRCheck' xs


-- mersenneTest :: IO [Integer]
mersenneTest n = do
                let p = primes !! n
                let m = 2^p -1
                b <- primeMR 2 m 
                if b then do
                    print p
                    mersenneTest (n+1)
                else 
                    mersenneTest (n+1)
                

