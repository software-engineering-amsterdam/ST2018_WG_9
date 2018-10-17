
module Lecture6

where 

import System.Random


-- <Exercise 1>
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
-- </Exercise 1>

-- <Exercise 3>
-- filter all non-primes from an infinte list starting at two
composites :: [Integer]
composites = filter (not . prime) [2..]
--  </Exercise 3>

-- <Exercise 4, Exercise 5, Exervise 6>
testFermatPrimality :: Int -> Integer -> IO Integer
testFermatPrimality k n = do bool <- primeTestsF k (composites !! fromIntegral n)
                             if not bool then testFermatPrimality k (fromIntegral n + 1)
                             else return (composites !! fromIntegral n)

-- Carmichael instead of composites
testFermatPrimality' :: Int -> Integer -> IO Integer
testFermatPrimality' k n = do bool <- primeTestsF k (carmichael !! fromIntegral n)
                              if not bool then testFermatPrimality' k (fromIntegral n + 1)
                              else return (carmichael !! fromIntegral n)

primeTestsF :: Int -> Integer -> IO Bool
primeTestsF k n = do as <- mapM (\ _ -> randomRIO (2, n - 1)) [1 .. k]
                     return (all (\ a -> exM a (n-1) n == 1) as)

-- infiniteTest takes a monadic function, Int 'k', and an Int 'n', replicates that function 'n' amount of times
-- with 'k' accuracy in the test function. Finally, it returns a list without any duplicates.
-- This function gives insight into the k values. 
infiniteTest :: (Monad m, Num t, Eq a) => (t1 -> t -> m a) -> t1 -> Int -> m [a]
infiniteTest f k n = do list <- replicateM n (f k 0)
                        let nubbed = nub list
                        return nubbed

-- 100 replicates is empirically determined by us, k can be variably assigned in the minumum runner below
infiniteTestCompsites, infiniteTestCarmichael, infiniteTestMillerRabin:: Int -> IO [Integer]
infiniteTestCompsites k = infiniteTest testFermatPrimality  k 100
infiniteTestCarmichael k = infiniteTest testFermatPrimality' k 100
infiniteTestMillerRabin k = infiniteTest testMillerRabin      k 100

-- K = 1, change source code to change K 
infiniteTestCompositesMin, infiniteTestCarmichaelMin, infiniteTestMillerRabinMin:: IO Integer
infiniteTestCompositesMin  = do list <- infiniteTestCompsites 1                               
                                return $ minimum list
infiniteTestCarmichaelMin  = do list <- infiniteTestCarmichael 1
                                return $ minimum list
infiniteTestMillerRabinMin = do list <- infiniteTestMillerRabin 1
                                return $ minimum list

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | k <- [2..], prime (6*k+1), prime (12*k+1), prime (18*k+1) ]

-- Using carmichael to test the Miller-Rabin primality check
testMillerRabin :: Int -> Integer -> IO Integer
testMillerRabin k n = do bool <- primeMR k (carmichael !! fromIntegral n)
                         if not bool then testMillerRabin k (fromIntegral n + 1)
                         else return (carmichael !! fromIntegral n)
-- </Exercise 4, Exercise 5, Exercise 6>

-- <Exercise 7>

-- Runner for Mersenne function
testMarsennePrimesRunner :: Int -> IO ()
testMarsennePrimesRunner k = discoverMarsennePrimes 0 k primes

-- Checks for the nth prime and higher if it is a Mersenne prime, printing the progress.
-- First argument 'c' is the counter
-- Second argument 'k' is the k used in Miller-Rabin primality check
-- Third argument 'list' is the list of primes
discoverMarsennePrimes :: Int -> Int -> [Integer] -> IO ()
discoverMarsennePrimes _ _ [] = print "Empty List"
discoverMarsennePrimes c k (x:xs) = do bool <- primeMR k (2^x - 1)
                                       if bool
                                       then do print ("#" ++ show c ++ " Marsenne number with (2^" ++ show x ++ ")-1 ") -- number: " ++ show (2^x - 1))
                                               discoverMarsennePrimes (c+1) k xs
                                       else discoverMarsennePrimes c k xs

-- We ran it for 45 minutes and we found 24 Mersenne primes.
-- "#0 Marsenne number with (2^2)-1 "
-- "#1 Marsenne number with (2^3)-1 "
-- "#2 Marsenne number with (2^5)-1 "
-- "#3 Marsenne number with (2^7)-1 "
-- "#4 Marsenne number with (2^13)-1 "
-- "#5 Marsenne number with (2^17)-1 "
-- "#6 Marsenne number with (2^19)-1 "
-- "#7 Marsenne number with (2^31)-1 "
-- "#8 Marsenne number with (2^61)-1 "
-- "#9 Marsenne number with (2^89)-1 "
-- "#10 Marsenne number with (2^107)-1 "
-- "#11 Marsenne number with (2^127)-1 "
-- "#12 Marsenne number with (2^521)-1 "
-- "#13 Marsenne number with (2^607)-1 "
-- "#14 Marsenne number with (2^1279)-1 "
-- "#15 Marsenne number with (2^2203)-1 "
-- "#16 Marsenne number with (2^2281)-1 "
-- ... etc
-- </Exercise 7>


factorsNaive :: Integer -> [Integer]
factorsNaive n0 = factors' n0 2 where 
  factors' 1 _ = []
  factors' n m 
    | n `mod` m == 0 = m : factors' (n `div` m) m
    | otherwise      =     factors' n (m+1)

factors :: Integer -> [Integer]
factors n0 = let
   ps0 = takeWhile (\ m -> m^2 <= n0) primes
 in factors' n0 ps0 where 
   factors' 1 _  = []
   factors' n [] = [n]
   factors' n (p:ps) 
    | n `mod` p == 0 = p: factors' (n `div` p) (p:ps)
    | otherwise      =    factors' n ps

prime :: Integer -> Bool
prime n = factors n == [n]

primes :: [Integer]
primes = 2 : filter prime [3..]

mers :: Integer -> Integer
mers 1  = 2^2-1;    mers 2  = 2^3-1;     mers 3  = 2^5-1
mers 4  = 2^7-1;    mers 5  = 2^13-1;    mers 6  = 2^17-1
mers 7  = 2^19-1;   mers 8  = 2^31-1;    mers 9  = 2^61-1
mers 10 = 2^89-1;   mers 11 = 2^107-1;   mers 12 = 2^127-1
mers 13 = 2^521-1;  mers 14 = 2^607-1;   mers 15 = 2^1279-1
mers 16 = 2^2203-1; mers 17 = 2^2281-1;  mers 18 = 2^3217-1
mers 19 = 2^4253-1; mers 20 = 2^4423-1;  mers 21 = 2^9689-1
mers 22 = 2^9941-1; mers 23 = 2^11213-1; mers 24 = 2^19937-1
mers 25 = 2^21701-1;
mers _  = undefined

bin2int :: [Int] -> Int
bin2int = bin . reverse where
  bin []  = 0
  bin [0] = 0
  bin [1] = 1
  bin (0:bs) = 2 * bin bs
  bin (1:bs) = 2 * bin bs + 1
  bin _      = error "not a binary digit list"

addM :: Integer -> Integer -> Integer -> Integer
addM x y = rem (x+y)

multM :: Integer -> Integer -> Integer -> Integer
multM x y = rem (x*y) 

invM :: Integer -> Integer -> Integer
invM x n = let 
   (u,v) = fctGcd x n
   copr  = x*u + v*n == 1
   i     = if signum u == 1 then u else u + n  
 in 
   if copr then i else error "no inverse"

fGcd :: Integer -> Integer -> Integer
fGcd a b = if b == 0 then a
                     else fGcd b (rem a b)

fctGcd :: Integer -> Integer -> (Integer,Integer) 
fctGcd a b = 
  if b == 0 
  then (1,0) 
  else 
     let 
       (q,r) = quotRem a b
       (s,t) = fctGcd b r 
     in (t, s - q*t)

coprime :: Integer -> Integer -> Bool
coprime n m = fGcd n m == 1

coprime' :: Integer -> Integer -> Bool
coprime' n m = let (x,y) = fctGcd n m
               in x*n + y*m == 1

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

grow :: (node -> [node]) -> node -> Tree node

grow step seed = T seed (map (grow step) (step seed))

takeT :: Int -> Tree a -> Tree a

takeT 0 (T x _) = T x []
takeT n (T x ts) = T x (map (takeT (n-1)) ts)

coprimeT :: Tree (Integer,Integer)
coprimeT = grow f (1,1) 

f :: (Integer,Integer) -> [(Integer,Integer)]
f (n,m) = [(n+m,m),(n,n+m)]

pairs :: [(Integer,Integer)]
pairs = concatMap (\ n -> zip [1..n] (repeat n)) [1..]

coprimes :: [(Integer,Integer)]
coprimes = filter (uncurry coprime) pairs

expM ::  Integer -> Integer -> Integer -> Integer
expM x y z = rem (x^y) z


primeTestF :: Integer -> IO Bool
primeTestF n = do 
   a <- randomRIO (2, n-1) :: IO Integer
   return (exM a (n-1) n == 1)

primeTestsF :: Int -> Integer -> IO Bool
primeTestsF k n = do
 as <- sequence $ fmap (\_-> randomRIO (2,n-1)) [1..k]
 return (all (\ a -> exM a (n-1) n == 1) as)

decomp :: Integer -> (Integer,Integer)
decomp n0 = decomp' (0,n0) where
  decomp' = until (odd.snd) (\ (m,n) -> (m+1,div n 2))

mrComposite :: Integer -> Integer -> Bool
mrComposite x n = let
    (r,s) = decomp (n-1)
    fs     = takeWhile (/= 1) 
       (map (\ j -> exM x (2^j*s) n)  [0..r])
  in 
    exM x s n /= 1 && last fs /= (n-1)

primeMR :: Int -> Integer -> IO Bool
primeMR _ 2 = return True
primeMR 0 _ = return True
primeMR k n = do 
    a <- randomRIO (2, n-1) :: IO Integer
    if exM a (n-1) n /= 1 || mrComposite a n
    then return False else primeMR (k-1) n


encodeDH :: Integer -> Integer -> Integer -> Integer
encodeDH p k m = m*k `mod` p

decodeDH :: Integer -> Integer -> Integer -> Integer -> Integer
decodeDH p ga b c = let 
    gab' = exM ga ((p-1)-b) p 
  in 
    rem (c*gab') p

encode :: Integer -> Integer -> Integer -> Integer
encode p k m = let 
   p' = p-1
   e  = head [ x | x <- [k..], gcd x p' == 1 ]
 in 
   exM m e p

decode :: Integer -> Integer -> Integer -> Integer
decode p k m = let 
   p' = p-1
   e  = head [ x | x <- [k..], gcd x p' == 1 ]
   d  = invM e p' 
 in 
   exM m d p

cipher :: Integer -> Integer
cipher = encode secret bound

decipher :: Integer -> Integer
decipher = decode secret bound

totient :: Integer -> Integer
totient n = toInteger $ length [ k | k <- [1..n], gcd k n == 1 ]

phi :: Integer -> Integer -> Integer
phi p q = (p - 1) * (q - 1)

select :: Integer -> Integer -> Integer
select p q = let
   t = phi p q 
 in
   head [ x | x <- [3..], gcd x t == 1 ]

rsaPublic :: Integer -> Integer -> (Integer,Integer)
rsaPublic p q = let
    e = select p q
  in
    (e,p*q)

rsaPrivate ::  Integer -> Integer -> (Integer,Integer)
rsaPrivate p q = let 
   e = select p q
   t = phi p q 
   d = invM e t
  in 
   (d,p*q)

rsaEncode :: (Integer,Integer) -> Integer -> Integer 
rsaEncode (e,n) m =  exM m e n

rsaDecode :: (Integer,Integer) -> Integer -> Integer 
rsaDecode = rsaEncode                              

trapdoor :: (Integer,Integer) -> Integer -> Integer
trapdoor = rsaEncode 

secret, bound :: Integer                
secret = mers 18
bound  = 131

