
module Lab3 where
 
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Data.Maybe
import Lecture3

-- | Exercise 1 - Time: 30min 

contradiction :: Form -> Bool
contradiction = not . satisfiable

tautology :: Form -> Bool
tautology f = all (`evl` f) (allVals f)

-- | logical entailment 
entails :: Form -> Form -> Bool
entails f1 f2 = tautology (Impl f1 f2)

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)

-- Form definitions for testing
entailTestForm1, entailTestForm2 :: Form
entailTestForm1 = Cnj [p,q]
entailTestForm2 = Dsj [Cnj [p,q],r]

equivTestForm1, equivTestForm2 :: Form
equivTestForm1 = Cnj [p,q]
equivTestForm2 = Cnj [q,p]

-- The implementation is fairly simple, and the test are pretty rudementary. However, due to the 

-- | Exercise 2 - Time: 25min
forms :: [Form]
forms = [form1, form2, form3]

testParser :: IO Bool
testParser = do let input = map show forms          -- Convert list of input to strings list of strings
                let result = concatMap parse input  -- Parse the input
                return (all (`elem` forms) result)  -- Compare to the initial list, if they're the same,
                                                    -- return true else false

-- Form generation (Exercise 4) can be used to get more input for testing purposes

-- | Exercise 3 - Time: 3,25 hours
-- Pipeline explanation
-- Step 1: use the equivalence between p→q and ¬p∨q to get rid of → symbols       |(arrowFree)
-- Step 2: use the equivalence of p↔q and (¬p∨q)∧(p∨¬q) to get rid of ↔ symbols   |(arrowFree)
-- Step 3: p↔q and (p∧q)∨(¬p∧¬q) is also equivalent                                |(arrowFree)
-- Step 4: conversion to negation normal form Use the equivalence between ¬¬ϕ and ϕ,| (nnf)
-- Step 5: Use the equivalence between ¬(ϕ∧ψ) and ¬ϕ∨¬ψ,                            | (nnf)
-- Step 6: Use the equivalence between ¬(ϕ∨ψ)and ¬ϕ∧¬ψ                              | (nnf)
-- Step 7: Distribute conjunction over disjunction (p^q) v r = (p^r)^(qvr)          |(cnf' -> dist)


cnf :: Form -> Form
cnf = flat . cnf' . nnf . arrowfree
    where cnf' :: Form -> Form
          cnf' (Cnj [f1,f2])       = Cnj [cnf' f1, cnf' f2]
          cnf' (Dsj [f1,f2])       = dist (head [cnf' f1, cnf' f2]) ([cnf' f1, cnf' f2] !! 1) 
          cnf' expr                = expr

          dist (Cnj [e11, e12]) e2 = Cnj [e11 `dist` e2, e12 `dist` e2] 
          dist e1 (Cnj [e21,e22])  = Cnj [e1 `dist` e21, e1 `dist` e22]
          dist e1 e2               = Dsj [e1,e2]

-- Step 6: associative propery: p ∧ q ∧ r ≡ (p ∧ q) ∧ r ≡ p ∧ ( q ∧ r)              | (flat)
-- For the list of conjunctions starting at the right form, iterate (foldr) with the empty list base
-- over the list of different subforms in the conjunction. If there is a case where one conjunction appears in this list,
-- the associative property above specifies that it can be concatenated here. If it is a different expression, nothing really
-- happens as it gets put in gets pushed on the resulting list. However, the recursive call will check that, and the rest, again.
-- Inspiration from reduction section here: https://stackoverflow.com/questions/16701186/recursion-in-treefold-function 

flat :: Form -> Form
flat (Cnj fs)        = Cnj $ foldr (f . flat) [] fs 
                     where f (Cnj xs) ys = xs ++ ys
                           f expr ys     = expr : ys
flat (Dsj fs)        = Dsj $ foldr (f . flat) [] fs
                     where f (Dsj xs) ys = xs ++ ys
                           f expr ys     = expr : ys
flat (Neg f)         = Neg (flat f)
flat expr            = expr


-- form 1 with different show
-- ((a==>b)<=>(-b==>-a))
-- After arrowfree
-- (((-a v b) ^ (--b v -a)) v (-(-a v b) ^ -(--b v -a)))
-- After nnf
-- (((-a v b) ^ (b v -a)) v ((a ^ -b) ^ (-b ^ a)))
-- After cnf'
-- (((((-a v b) v a) ^ ((-a v b) v -b)) ^ (((-a v b) v -b) ^ ((-a v b) v a))) ^ ((((b v -a) v a) ^ ((b v -a) v -b)) ^ (((b v -a) v -b) ^ ((b v -a) v a))))
-- After flatten
-- ((-a v b v a) ^ (-a v b v -b) ^ (-a v b v -b) ^ (-a v b v a) ^ (b v -a v a) ^ (b v -a v -b) ^ (b v -a -b) ^ (b v -a v a))

-- The next step would be to prune things like (-a v b v a), (tautology, always true). Although an attempt was made, I spent more time on Exercise 4 as that was more
-- productive for the grading.

-- | Exercisse 4 - Time: 1 hour
-- Argument n determines 'levels' of the form. Random literals between 0 and 10 get replaced by 
-- new logical operators (genSubForm)
-- genForm :: Int -> IO Form 
-- genForm n = do randomLength <- getRandomInt 10
--                new <- genForm' n (map Prop [1 .. (randomLength + 1)])
--                return (Cnj new)

-- genForm' :: Int -> [Form] -> IO [Form]
-- genForm' 0 xs = return xs 
-- genForm' l xs = do newForms <- mapM genSubForm xs
--                    genForm' (l-1) newForms

-- genSubForm :: Form -> IO Form
-- genSubForm (Prop x)   = do n <- getRandomInt 5
--                            return (substituteLiterals n (Prop x))
-- genSubForm (Impl a b) = do newFormA <- genSubForm a
--                            newFormB <- genSubForm b
--                            return (Impl newFormA newFormB)
-- genSubForm (Dsj xs)   = do newForms <- mapM genSubForm xs
--                            return (Dsj newForms)
-- genSubForm (Cnj xs)   = do newForms <- mapM genSubForm xs
--                            return (Cnj newForms)
-- genSubForm (Neg x)    = do newForm <- genSubForm x
--                            return (Neg newForm)
-- genSubForm f          =    return f

-- substituteLiterals :: Int -> Form -> Form
-- substituteLiterals 0 (Prop n) = Dsj   [Prop n, Prop (n + 1)]
-- substituteLiterals 1 (Prop n) = Cnj   [Prop n, Prop (n + 1)]
-- substituteLiterals 2 (Prop n) = Impl  (Prop n) (Prop (n + 1))
-- substituteLiterals 3 (Prop n) = Equiv (Prop n) (Prop (n + 1))
-- substituteLiterals 4 (Prop n) = Neg   (Prop n)
-- substituteLiterals 5 (Prop n) = Prop   n

-- getRandomInt :: Int -> IO Int
-- getRandomInt n = getStdRandom (randomR (0,n))

-- -- Checks if a Form is cnf based on CNF's properties (conjuctions with one level of disjunctions.
-- isCNF :: Form -> Bool
-- isCNF (Cnj xs) = all isDis xs || all isLit xs
--                   where isDis (Dsj xs)       = all isLit xs 
--                         isDis _              = False
--                         isLit (Prop n)       = True
--                         isLit (Neg (Prop n)) = True
--                         isLit _              = False
-- isCNF _                                      = False

-- -- The properties tested are isCNF and logical equivelance.
-- -- logical equivelance is a weaker property than isCNF. Logical equivalence can be achieved by any logical permutation.
-- testR :: Int -> Int -> IO ()
-- testR k n = if k == n then print (show n ++ " tests passed")
--                 else do
--                   xs <- genForm 3
--                   let cnfver = cnf xs
--                   if isCNF cnfver && equiv xs cnfver then
--                     do print ("pass on: " ++ show xs)
--                        testR (k+1) n
--                   else error ("failed test " ++ intToDigit(k) : " on: " ++ show xs)