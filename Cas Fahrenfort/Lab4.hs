module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck 
import SetOrd

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = do
        k <- choose (0 :: Int, 30 :: Int)
        t <- sequence [ arbitrary | _ <- [1..k] ]
        return (list2set t)

propSet :: Set Int -> Bool
propSet (Set xs) = list2set xs == Set xs

propSetDuplicates :: Set Int -> Bool
propSetDuplicates (Set [])  = True
propSetDuplicates (Set [x]) = True
propSetDuplicates (Set (x:xs)) | x `elem` xs = False
                               | otherwise = propSetDuplicates (Set xs)