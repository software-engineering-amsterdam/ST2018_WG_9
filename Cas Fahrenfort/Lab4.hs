module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck 
import SetOrd

set2list :: Set a -> [a]
set2list (Set xs) = xs

-- Exercise 1 --

-- 1) Antisymmetry is still somewhat unclear to me
-- 2) Is {a, b, b} a valid set?
-- 3) 

-- Exercise 2 --
-- 1 hour

-- Arbitrary instance for Set a.
-- Apply list2set to an arbitrary list of a.
instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = list2set <$> arbitrary

-- Test whether the Set is ordered from low to high.
-- This is not an official property of sets, but it is in the implementation we are using.
-- This test succeeds if the set contains duplicates but is ordered (e.g. Set [1,2,2,3]).
-- However, duplicates are tested by another property.
propOrdered :: Set Int -> Bool
propOrdered (Set xs) = sort xs == xs

-- Test whether the set contains any duplicates.
propSetDuplicates :: Set Int -> Bool
propSetDuplicates (Set [])  = True
propSetDuplicates (Set [x]) = True
propSetDuplicates (Set (x:xs)) | x `elem` xs = False
                               | otherwise = propSetDuplicates (Set xs)

-- quickCheck runner for the above two properties.
-- Returns "+++ OK, passed 100 tests" twice
ex2quickCheck :: IO ()
ex2quickCheck = do
                putStrLn "propOrdered"
                quickCheck propOrdered
                putStrLn "propSetDuplicates"
                quickCheck propSetDuplicates

-- Exercise 3 --
-- 30 minutes

-- Set union has already been defined in SetOrd.hs!

intersectSet :: Eq a => Set a -> Set a -> Set a
intersectSet (Set xs) (Set ys) = Set (xs `intersect` ys) 

diffSet :: Eq a => Set a -> Set a -> Set a
diffSet (Set xs) (Set ys) = Set (xs \\ ys)

-- Set union is defined as: A ∪ B = {x | x ∈ A or x ∈ B}
-- This property tests for all elements of the result of 'unionSet' whether they are
-- an element of A or an element of B.
propSetUnion :: Set Int -> Set Int -> Bool
propSetUnion (Set a) (Set b) = all (\x -> x `elem` a || x `elem` b) union
    where union = set2list $ unionSet (Set a) (Set b)

-- Set intersection is defined as: A ∩ B = {x | x ∈ A and x ∈ B}
-- This property tests for all elements of the result of 'intersectSet' whether they are
-- an element of A and an element of B.
propSetIntersect :: Set Int -> Set Int -> Bool
propSetIntersect (Set a) (Set b) = all (\x -> x `elem` a && x `elem` b) intersect
    where intersect = set2list $ intersectSet (Set a) (Set b)

-- Set difference is defined as: A – B = {x | x ∈ A and x ∉ B}
-- This property tests for all elements of the result of 'diffSet' whether they are
-- an element of A and not an element of B.
propSetDiff :: Set Int -> Set Int -> Bool
propSetDiff (Set a) (Set b) = all (\x -> x `elem` a && x `notElem` b) diff
    where diff = set2list $ diffSet (Set a) (Set b)

-- quickCheck runner for the above three properties.
-- Returns "+++ OK, passed 100 tests" thrice
ex3quickCheck :: IO ()
ex3quickCheck = do
                putStrLn "propSetUnion"
                quickCheck propSetIntersect
                putStrLn "propSetIntersect"
                quickCheck propSetIntersect
                putStrLn "propSetDiff"
                quickCheck propSetDiff

-- Exercise 5 --
-- 10 minutes

-- Define Rel as a Set, to keep using Set operations.
type Rel a = Set (a,a)

-- The symmetric closure of a relation R is defined as: R ∪ R⁻¹.
symClos :: Ord a => Rel a -> Rel a
symClos s = unionSet s (inverse s)

-- Invert a relation
inverse :: Rel a -> Rel a
inverse (Set rs) = Set (map invert rs)

invert :: (a, a) -> (a, a)
invert (x, y) = (y, x)

-- Exercise 6 --
-- 45 minutes

testSet :: Rel Int
testSet = Set [(1,2),(2,3),(3,4)] 

-- Relational composition
infixr 5 @@
 
(@@) :: Eq a => Rel a -> Rel a -> Rel a
(Set r) @@ (Set s) = Set $ nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- The transitive closure of a relation R is by definition the smallest transitive relation S such that R ⊆ S.
-- This function recursively applies the @@ function to a set and unions the result of this operation with the set itself,
-- adding all the transitive relations from the set to the set every iteration.
-- Once the difference of the transitivity of the set with the accumulator is empty, we know nothing new is being added to the
-- accumulator, so we can terminate the function.
trClos :: Ord a => Rel a -> Rel a 
trClos set = clos set set
    where clos rel acc | diffSet (rel @@ acc) acc == Set [] = acc
                       | otherwise = clos (rel @@ acc) (unionSet (rel @@ acc) acc)

-- Exercise 7 --
-- 30 mins
-- Test report: I thought of properties on symmetrical closures and transitive closures, then defined those
-- as functions of type Rel Int -> Bool, to use them with quickCheck. In 'propTrClos', the method used for checking
-- if the Relation is transitive is very similar to the definition of relational composition (@@) above, which comes
-- close to using the definition of the function as as a property to test the result.

-- By definition of a symmetric closure, for any relation in the set, the inverse of the relation
-- is also in the set.
propSymClos :: Rel Int -> Bool
propSymClos set = all (\x -> invert x `elem` sc) sc
    where sc = set2list $ symClos set

-- Property which checks if a relation is transitive.
propTrClos :: Rel Int -> Bool
propTrClos set = and [(x, w) `elem` tc | (x, y) <- tc, (z, w) <- tc, y == z]
    where tc = set2list $ trClos set

-- quickCheck runner for the above two properties.
-- Returns "+++ OK, passed 100 tests" twice
ex7quickCheck :: IO ()
ex7quickCheck = do
                putStrLn "propSymClos"
                quickCheck propSymClos
                putStrLn "propTrClos"
                quickCheck propTrClos

-- Exercise 8 --
-- 15 minutes

-- The closures differ from each other. This function calculates both closures of the testSet,
-- then shows the difference.
closureDiff :: IO ()
closureDiff = do
    let ts = trClos (symClos testSet)
    let st = symClos (trClos testSet)
    putStrLn $ "A = " ++ show ts
    putStrLn $ "B = " ++ show st
    putStrLn $ "A - B = " ++ show (diffSet ts st)