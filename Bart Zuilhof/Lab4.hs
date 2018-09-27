module Lab4 where
import Data.List
import Test.QuickCheck    

import SetOrd
import Lecture2

-- Exercise 2
-- Time spent: 30min
instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = list2set <$> arbitrary

quickCheckTestSet :: Set Int -> Bool
quickCheckTestSet xs =  checkDuplicates' xs && checkSorted xs

checkDuplicates' :: Set Int -> Bool 
checkDuplicates' x = checkDuplicates x []

checkDuplicates :: Set Int -> [Int] -> Bool
checkDuplicates (Set []) _ = True
checkDuplicates (Set (x:xs)) seen   | x `elem` seen = False
                                    | otherwise = checkDuplicates (Set (xs)) (x:seen)
checkSorted :: Set Int -> Bool 
checkSorted (Set xs) = sort xs == xs

-- Exercise 3
-- Time spent: 60min

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set x) (Set y) = Set (x `intersect` y)

testIntersection :: Set Int -> Set Int -> Bool
testIntersection (Set xs) (Set ys) = all (\i -> elem i xs && elem i ys) intersection && all (\i -> not(i `elem` ys) || elem i ys ) xs
                    where intersection = setToList (setIntersection (Set xs) (Set ys))

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion = unionSet

testUnion :: Set Int -> Set Int -> Bool
testUnion (Set xs) (Set ys) = all (\u -> elem u xs || elem u ys) union
    where union = setToList (setUnion (Set xs) (Set ys))

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set xs) (Set ys) = Set (xs \\ ys)

testDifference :: Set Int -> Set Int -> Bool
testDifference (Set xs) (Set ys) = all (`notElem` ys) difference
    where difference = setToList (setDifference (Set xs) (Set ys))

setToList :: Set a -> [a]
setToList (Set xs) = xs                       

-- Exercise 5
-- Time spent: 10min
type Rel a = Set(a,a)


symClos :: Ord a => Rel a -> Rel a
symClos xs = setUnion xs ys
        where ys = inverseRelation xs

inverseRelation :: Rel a -> Rel a
inverseRelation (Set xs) = Set (map (\(x,y)-> (y,x)) xs)

-- Exercise 6
-- Time spent: 60min
testSet = Set[(1, 2), (2, 3), (3, 4)]
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
(Set r) @@ (Set s) = Set (nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ])

trClos :: Ord a => Rel a -> Rel a
trClos (Set xs) = trClos' (Set xs) (Set xs) 

trClos' :: Ord a => Rel a -> Rel a -> Rel a
trClos' rel acc | setDifference (rel @@ acc) acc == Set [] = acc
                | otherwise = trClos' (rel @@ acc) (setUnion acc (rel @@ acc))

-- Exercise 7  
-- Time spent: 15min              --                                
testSymClos :: Ord a => Rel a -> Bool
testSymClos rel = all (\(x,y) -> elem (y,x) symClosure) symClosure
        where symClosure = setToList (symClos rel)

testTransClos :: Ord a => Rel a -> Bool
testTransClos rel = (transClosure @@ transClosure) == transClosure
        where transClosure = trClos rel

-- Exercise 8
-- Time spent: 5min 
-- applying the both the combinations of the compositions of the relations transitive- and symetric closure to the testset,
-- results into two different sets. Therefore these relations are  not commutative
exercise8answer :: Bool
exercise8answer = (symClos . trClos) testSet == (trClos. symClos) testSet
-- evaluates to FALSE