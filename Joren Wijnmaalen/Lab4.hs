module Lab4 where
 
import Data.List
import System.Random
import Test.QuickCheck
import SetOrd


-- EXERCISE 2 --
-- 30 mins
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

-- Runner
-- Result: +++ OK, passed 100 tests.
quickCheckSetRunner :: IO ()
quickCheckSetRunner = quickCheck quickCheckSet

-- Exercise 3
-- 30 mins
intersectSet :: Eq a => Set a -> Set a -> Set a
intersectSet (Set xs) (Set ys) = Set (filter (`elem` ys) xs)

unionSet' :: (Ord a, Eq a) => Set a -> Set a -> Set a
unionSet' (Set xs) (Set ys) = list2set (xs ++ ys)

differSet :: Eq a => Set a -> Set a -> Set a
differSet (Set xs) (Set ys) = Set (filter (not . (`elem` ys)) xs)

-- relational union property given by A ∪ B = { x | x ∈ A ∨ x ∈ B }
-- params: input -> output 
isUnion :: Eq a => (Set a, Set a) -> Set a -> Bool
isUnion ((Set xs), (Set ys)) (Set zs) = all (`elem` zs) xs && all (`elem` zs) ys

-- A − B = { x | x ∈ A ∧ x /∈ B }
isDifference :: Eq a => (Set a, Set a) -> Set a -> Bool
isDifference ((Set xs), (Set ys)) (Set zs) = all (\x -> (elem x xs) && not (elem x ys)) zs

-- A ∩ B = { x | x ∈ A ∧ x ∈ B }
isIntersection :: Eq a => (Set a, Set a) -> Set a -> Bool
isIntersection ((Set xs), (Set ys)) (Set zs) = all (\x -> elem x xs && elem x ys) zs


-- Run the union test with quickcheck
-- Result: +++ OK, passed 100 tests.
unionTest :: Set Int -> Set Int -> Bool
unionTest a b = isUnion (a, b) (unionSet' a b)

unionTestRunner :: IO ()
unionTestRunner = quickCheck unionTest

-- Run the difference test with quickcheck
-- Result: +++ OK, passed 100 tests.
differenceTest :: Set Int -> Set Int -> Bool
differenceTest a b = isDifference (a, b) (differSet a b)

differenceTestRunner :: IO ()
differenceTestRunner = quickCheck differenceTest

-- Run the intersection test with quickcheck
-- Result: +++ OK, passed 100 tests.
intersectionTest :: Set Int -> Set Int -> Bool
intersectionTest a b = isIntersection (a, b) (intersectSet a b)

intersectionTestRunner :: IO ()
intersectionTestRunner = quickCheck intersectionTest

-- Exercise 4



-- Exercise 5
-- 10 mins
type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos x = x ++ inv x
        where inv x = map (\(a,b) -> (b,a)) x

-- Exercise 6
-- 45 mins
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

r = [(1,2),(2,3),(3,4)]
transR = [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

-- ∀xyz (xRy ∧ yRz ⇒ xRz)
-- For every (a,b) in the relation, find all (c, d)'s where b == c
-- If such elements exist another element (a, d) should also exist for the relation to be transitive
isTransitive :: Eq a => Rel a -> Bool
isTransitive xs = all (\(a,b) -> all (\(c, d) -> elem (a,d) xs) (findIndirect (a,b) xs)) xs
        where findIndirect (a, b) xs = filter (\(c,d) -> b == c) xs

-- using the property transitive closure S = R U R2 U R3 ... U Rk
-- for the smalles k finds the transitive closure.
-- Params: - orig, the original relation to compose Rk with (Rk = R.Rk-1)
--         - result, the union that recursively builds up
--         - r, Rk to construct RK+1 with 
-- Result: trCol r == transR
trCol :: Ord a => Rel a -> Rel a
trCol xs = trCol' xs xs xs
    where trCol' orig result r | isTransitive result = result
                               | otherwise = trCol' orig (result ++ (orig @@ r)) (orig @@ r) 
                            

-- Exercise 7
-- 30 mins

-- PROPERTIES --
-- For b to be a closure of a, a shoulde be a subset implying smaller than or equal
closureProp :: Rel a -> Rel a -> Bool
closureProp a b = length a <= length b

-- The logical definition of subset is as follows forall ∀x(x ∈ A ⇒ x ∈ B)
closureProp2 :: Eq a => Rel a -> Rel a -> Bool
closureProp2 a b = all (`elem` b) a

isSymClos :: Eq a => Rel a -> Bool
isSymClos xs = all (\(a,b) -> elem (b,a) xs) xs

-- TESTS --
testTrCol :: Rel Int -> Bool
testTrCol a = isTransitive (trCol a) &&  closureProp a (trCol a) && closureProp2 a (trCol a)

-- Returns: True
testTrColManualRunner :: Bool
testTrColManualRunner = testTrCol r

-- Quickcheck already knows how to randomly generate lists of tuples
-- Result: +++ OK, passed 100 tests.
testTrColRunner :: IO ()
testTrColRunner = quickCheck testTrCol

-- Test for symmetric closure
testSymClos :: Rel Int -> Bool
testSymClos a = isSymClos (symClos a) && closureProp a (symClos a) && closureProp2 a (symClos a)

-- Returns: True
testSymClosManualRunner :: Bool
testSymClosManualRunner = testSymClos r

-- Result: +++ OK, passed 100 tests.
testSymClosRunner :: IO ()
testSymClosRunner = quickCheck testSymClos

-- Exercise 8 -- 
testAssumption = [(1,2), (3,4)]

-- The order of the closure appliance matters.
-- trCol . symClos $ testR = [(1,2),(3,4),(2,1),(4,3),(1,1),(3,3),(2,2),(4,4)]
-- symClos . trCol $ testR = [(1,2),(3,4),(2,1),(4,3)]
-- This is a counter example so the lemma doesn't hold.