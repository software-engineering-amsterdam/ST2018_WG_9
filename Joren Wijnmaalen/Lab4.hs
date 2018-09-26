module Lab4 where
 
import Data.List
import System.Random
import Test.QuickCheck
import SetOrd


-- EXERCISE 2 --
instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = do
        k <- choose(0 :: Int, 10 :: Int)
        ts <- sequence [ arbitrary | _ <- [0..k]]
        return (list2set ts)

quickCheckSet :: Set Int -> Bool
quickCheckSet = not . hasDuplicates

hasDuplicates :: Set Int -> Bool
hasDuplicates (Set []) = False
hasDuplicates (Set (x:xs)) | x `elem` xs = True
                     | otherwise = hasDuplicates (Set xs)