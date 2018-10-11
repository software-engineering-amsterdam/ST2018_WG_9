module Lab6 where
 
import Data.List
import Data.Bits
import System.Random
import Lecture6


-- that does modular exponentiation of xy in polynomial time, by repeatedly squaring modulo N.
-- exM :: Integer -> Integer -> Integer -> Integer
-- exM b 0 m = 1
-- exM b e m = t * exM ((b * b) `mod` m) (shiftR e 1) m `mod` m
--            where t = if testBit e 0 then b `mod` m else 1