module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck 
import SetOrd

set2list :: Set a -> [a]
set2list (Set xs) = xs

-- | Exercise 1 --

-- 1) Antisymmetry is still somewhat unclear to me
-- 2) Is {a, b, b} a valid set? 
-- 3) Does symmetry imply reflectivity?

-- | Exercise 2 --

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

-- | Exercise 3 --

-- Set union has already been defined in SetOrd.hs!
-- For your benefit, we included another implementation.
unionSet' :: (Ord a) => Set a -> Set a -> Set a
unionSet' = unionSet

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
                quickCheck propSetUnion
                putStrLn "propSetIntersect"
                quickCheck propSetIntersect
                putStrLn "propSetDiff"
                quickCheck propSetDiff
-- | Exercise 4
-- Nothing really catched our eye that we couldn't figure out ourselves. 

-- | Exercise 5 --

-- Define Rel as a Set, to keep using Set operations and to be able to use the above implemented
-- operations.
type Rel a = Set (a,a)

-- The symmetric closure of a relation R is defined as: R ∪ R⁻¹.
symClos :: Ord a => Rel a -> Rel a
symClos s = unionSet s (inverse s)

-- Invert a relation
inverse :: Rel a -> Rel a
inverse (Set rs) = Set (map invert rs)

invert :: (a, a) -> (a, a)
invert (x, y) = (y, x)

-- | Exercise 6 --

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
trClos set = apprx [foldl1 unionSet (take n rs) | n <- [1..]] 
           where rs             = iterate (set @@) set
                 apprx (x:y:zs) = if x == y then x else apprx (y:zs)

-- | Exercise 7 --
-- Test report: I thought of properties on symmetrical closures and transitive closures, then defined those
-- as functions of type Rel Int -> Bool, to use them with quickCheck. In 'propTrClos', the method used for checking
-- if the Relation is transitive is very similar to the definition of relational composition (@@) above, which comes
-- close to using the definition of the function as as a property to test the result.

-- Closure properties
-- For b to be a closure of a, a shoulde be a subset implying smaller than or equal
propClosure :: Rel a -> Rel a -> Bool
propClosure (Set a) (Set b) = length a <= length b

-- The logical definition of subset is as follows ∀x(x ∈ A ⇒ x ∈ B)
propClosure2 :: Eq a => Rel a -> Rel a -> Bool
propClosure2 (Set a) (Set b) = all (`elem` b) a

-- By definition of a symmetric closure, for any relation in the set, the inverse of the relation
-- is also in the set. Also test the general closure properties.
propSymClos :: Rel Int -> Bool
propSymClos set = all (\x -> invert x `elem` sc) sc && propClosure set (Set sc) && propClosure2 set (Set sc)
    where sc = set2list $ symClos set

-- Property which checks if a relation is transitive. Also test the general closure properties.
propTrClos :: Rel Int -> Bool
propTrClos set = and [(x, w) `elem` tc | (x, y) <- tc, (z, w) <- tc, y == z] && propClosure set (Set tc) && propClosure2 set (Set tc)
    where tc = set2list $ trClos set

-- quickCheck runner for the above two properties.
-- Returns "+++ OK, passed 100 tests" twice
ex7quickCheck :: IO ()
ex7quickCheck = do
                putStrLn "propSymClos"
                quickCheck propSymClos
                putStrLn "propTrClos"
                quickCheck propTrClos

-- | Exercise 8 --

testSet :: Rel Int
testSet = Set [(1,2),(2,3),(3,4)] 

-- The closures differ from each other. This function calculates both closures of the testSet,
-- then shows the difference. The difference is not the empty set, therefore the order of application
-- matters. This is a counter example, the lemma does not hold.
closureDiff :: IO ()
closureDiff = do let ts = (trClos . symClos) testSet
                 let st = (symClos . trClos) testSet
                 putStrLn $ "A = "     ++ show ts
                 putStrLn $ "B = "     ++ show st
                 putStrLn $ "A - B = " ++ show (diffSet ts st)