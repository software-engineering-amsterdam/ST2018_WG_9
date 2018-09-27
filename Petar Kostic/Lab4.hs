module Lab4 where
 
import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

-- | Exercise 1



-- | Exercise 2 - 40 min
instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = list2set <$> arbitrary

noDuplicates :: Set Int -> Bool
noDuplicates (Set [])     = True
noDuplicates (Set (x:xs)) =  x `notElem` xs && noDuplicates (Set xs)

isOrdered :: Set Int -> Bool
isOrdered (Set [])       = True
isOrdered (Set [x])      = True
isOrdered (Set (x:y:xs)) = x <= y && isOrdered (Set (y:xs))

-- | Exercise 3 - 50 min 
intersectSet :: (Ord a) => Set a -> Set a -> Set a 
intersectSet (Set xs) (Set ys) = Set (xs `intersect` ys)

differenceSet :: (Ord a) => Set a -> Set a -> Set a 
differenceSet (Set xs) (Set ys) = Set (xs \\ ys)

unionSet' :: (Ord a) => Set a -> Set a -> Set a
unionSet' = unionSet

propIntersectSet :: Set Int -> Set Int -> Bool
propIntersectSet set1@(Set xs) set2@(Set ys) = propIntersectSet' (intersectSet set1 set2)
        where propIntersectSet' (Set [])     = True
              propIntersectSet' (Set (z:zs)) = z `elem` xs && z `elem` ys && propIntersectSet' (Set zs) 

propUnionSet :: Set Int -> Set Int -> Bool
propUnionSet set1@(Set xs) set2@(Set ys) = propUnionSet' (unionSet' set1 set2)
        where propUnionSet'     (Set [])     = True
              propUnionSet'     (Set (z:zs)) = (z `elem` xs || z `elem` ys) && propUnionSet' (Set zs)

propDifferenceSet :: Set Int -> Set Int -> Bool
propDifferenceSet set1@(Set xs) set2@(Set ys) = propDifferenceSet' (differenceSet set1 set2)
        where propDifferenceSet' (Set [])     = True
              propDifferenceSet' (Set (z:zs)) = (z `elem` xs && z `notElem` ys) && propDifferenceSet' (Set zs)

-- | Exercise 5 - 15 min

type Rel a = Set (a,a)

-- Gives the symmetric closure of a relation, where the relation is represented as an ordered list of pairs. 
-- E.g., symClos [(1,2),(2,3),(3,4)] should give  [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)].
symClos :: Ord a => Rel a -> Rel a
symClos set = unionSet set (inverseRel set) 
    
inverseRel :: Rel a -> Rel a
inverseRel (Set rs) = Set (map (\ (x, y) -> (y, x)) rs)

-- Exercise 6 - 
infixr 5 @@
-- Removes all transitive relationshops 
(@@) :: Eq a => Rel a -> Rel a -> Rel a
(Set r) @@ (Set s) = Set (nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ])

-- trClos [(1,2),(2,3),(3,4)] should give  [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].
trClos' :: Ord a => Int -> Rel a -> [Rel a] 
trClos' set@(Set rs) n = takeWhile ((state n `setDifference` (state n + 1)) == Set [])
                        where state = iterate (set @@) set

trClos :: Ord 

testSet :: Rel Int
testSet = Set [(1,2),(2,3),(3,4)]

main :: IO ()
main = do print "-------------- EXERCISE 2 -----------------"
          print "Exercise 2 - Generator"
          generate arbitrary :: IO (Set Int)
          print "Exercise 2 - Property: noDuplicates"
          quickCheck noDuplicates
          print "Exercise 2 - Property: isOrdered"
          quickCheck isOrdered
          print "-------------- EXERCISE 3 -----------------"
          print "Exercise 3 - Property: intersectSet"
          quickCheck propIntersectSet
          print "Exercise 3 - Property: unionSet"
          quickCheck propUnionSet
          print "Exercise 3 - Property: propDifferenceSet"
          quickCheck propDifferenceSet
          print "-------------- EXERCISE 5 -----------------"
          