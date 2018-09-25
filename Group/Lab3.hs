module Lab3 where

import Data.List
import System.Random
-- import System.IO.Unsafe
-- import Data.IORef
import Test.QuickCheck
import Lecture3

-- Exercise 1
-- 1 hour

-- A contradiction means a formula is not satisfiable.
-- This can be checked with the formula below.
form4 = Cnj [p, Neg p]

contradiction :: Form -> Bool
contradiction = not . satisfiable

-- A tautology means for all valuations, a formula evaluates to True.
-- This can be checked with the formula below.
form5 = Dsj [p, Neg p]

tautology :: Form -> Bool
tautology f = all (`evl` f) (allVals f)

-- Entailment means all the valuations that satisfy B also satisfy A (a.k.a logical implication).
-- To check this, we assuma f1 implies f2, then see if for all evaluations the implication
-- evaluates to True (i.e. tautology). This can be checked with the formulas below.
form6 = Cnj [p, q]
form7 = Dsj [Cnj [p, q], r]

entails :: Form -> Form -> Bool
entails f1 f2 = tautology (Impl f1 f2)

-- Equivalence means for all valuations, f1 and f2 evaluate to the same value.
-- To check this, we assume f1 and f2 are equivalent, then see if for all evaluations
-- the equivalence evaluates to True (i.e. tautology). This can be checked with
-- the formulas below.
form8 = Cnj [p, q]
form9 = Cnj [q, p]

equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)

-- Exercise 2
-- 15 minutes
-- The only way to test the implementation is by using test cases. We use the show function
-- to convert predefined forms to a string, then feed the string into the parse function
-- and see if the result is equal to the original formula.

forms = [form1, form2, form3, form4, form5, form6, form7, form8, form9]

-- This function returns true if for all formulas in the 'forms' list, the result of
-- the 'parse' function applied to the 'show' of the formula is equal to the original formula.
testParse :: IO Bool
testParse = do let fs = map show forms
               let ps = concatMap parse fs
               return $ ps == forms

-- Using the implementation from exercise 4, we can generate a list of forms

-- testParse' :: Int -> IO Bool 
-- testParse' n = do let fs = map show (genRandFormList n)
--                   let ps = concatMap parse fs
--                   return $ ps == forms

-- genRandFormList :: Int -> [IO Form]
-- genRandFormList 0 l = []
-- genRandFormList n l = formGenerator 3 : genRandFormList (n-1)

-- However, since we need the Show instance to work and the generator is written for IO
-- Something like 
-- instance (Show a) => Show (IORef a) where
--     show a = show (unsafePerformIO (readIORef a))
-- can be used, but is not good practice. The best thing to do is to write 
-- a new generator outside the IO monad again.

-- Exercise 3
-- 1 hour. This implementation for cnf looks at the truthtable. All rows that result in false are conjuncted,
-- negated and conjuncted again resulting in a cnf when resolving the negation to the conjunction clauses.
-- Example:
--  p   q   phi
--  0   0   0
--  0   1   0
--  1   0   1
--  1   1   1
-- Evals that makes phi false: [(p = 0, q = 0), (p = 0, q = 1)].
-- Convert to clause: [(-p ^ -q), (-p ^ q)]
-- Negated: [-(-p ^ -q) <=> (p v q), -(-p ^ q) <=> (p v -q)]
--          [(p v q), (p v -q)]
-- Outer conjunction: ((p v q) ^ (p v -q)) :: CNF


toCNF :: Form -> Form
toCNF form = Cnj (map valToForm (relevantVals form))
    where relevantVals form = filter (\x -> not (evl x form)) (allVals form)
            
    
valToForm :: Valuation -> Form    
valToForm v = valToForm' v []
    where valToForm' [] res = nnf (Neg(Cnj res)) 
          valToForm' ((n, False):xs) res = valToForm' xs (Neg (Prop n) : res)
          valToForm' ((n, True):xs) res = valToForm' xs (Prop n : res)

-- Bonus: Version without Truth tables
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


-- Form1 with different show, note that toCNF cannot convert form1 to CNF because it is a tautology
-- ((a==>b)<=>(-b==>-a))
-- After arrowfree
-- (((-a v b) ^ (--b v -a)) v (-(-a v b) ^ -(--b v -a)))
-- After nnf
-- (((-a v b) ^ (b v -a)) v ((a ^ -b) ^ (-b ^ a)))
-- After cnf'
-- (((((-a v b) v a) ^ ((-a v b) v -b)) ^ (((-a v b) v -b) ^ ((-a v b) v a))) ^ ((((b v -a) v a) ^ ((b v -a) v -b)) ^ (((b v -a) v -b) ^ ((b v -a) v a))))
-- After flatten
-- ((-a v b v a) ^ (-a v b v -b) ^ (-a v b v -b) ^ (-a v b v a) ^ (b v -a v a) ^ (b v -a v -b) ^ (b v -a -b) ^ (b v -a v a))

-- The next step would be to prune things like (-a v b v a), (tautology, always true). Although an attempt was made, we spent more time on Exercise 4 as that was more
-- productive for the grading.

-- Exercise 4
-- 1.5 hours
-- The integer input specifices the amount of recursive calls. The algorithm starts with a list of literals
-- of random length between 0 and 10. Each recursive call, the expression tree is traversed where every literal
-- is substituted by a random new logical operator (see the substituteLiterals function).

formGenerator :: Int -> IO Form
formGenerator levels =  do  randomLength <- getRandomInt 10
                            new <- formGenerator' levels(map Prop [1 .. (randomLength + 1)])
                            return $ Cnj new

formGenerator' :: Int -> [Form] -> IO [Form]
formGenerator' 0 xs = return xs
formGenerator' l xs = do newForms <- mapM subformGenerator xs
                         formGenerator' (l-1) newForms 

-- Patternmatches the current form, if it is a literal: Substitute it for a random other logical relation
-- Otherwise, recursively traverse deeper into the expression
subformGenerator :: Form -> IO Form
subformGenerator (Prop x)   = do n <- getRandomInt 5
                                 return $ substituteLiterals n (Prop x)
subformGenerator (Impl a b) = do newFormA <- subformGenerator a
                                 newFormB <- subformGenerator b
                                 return $ Impl newFormA newFormB
subformGenerator (Dsj xs)   = do newForms <- mapM subformGenerator xs
                                 return $ Dsj newForms
subformGenerator (Cnj xs)   = do newForms <- mapM subformGenerator xs
                                 return $ Cnj newForms
subformGenerator (Neg x)    = do newForm <- subformGenerator x
                                 return $ Neg newForm     
subformGenerator f = return f                      

substituteLiterals :: Int -> Form -> Form
substituteLiterals 0 (Prop n) = Dsj [Prop n, Prop (n + 1)]
substituteLiterals 1 (Prop n) = Cnj [Prop n, Prop (n + 1)]
substituteLiterals 2 (Prop n) = Impl (Prop n) (Prop (n + 1))
substituteLiterals 3 (Prop n) = Equiv (Prop n) (Prop (n + 1))
substituteLiterals 4 (Prop n) = Neg (Prop n)
substituteLiterals 5 (Prop n) = Prop n

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

-- The cnf property, a form is in cnf if it has an outer conjunction, and one level of disjunctions.
-- The disjunctions should contain lists.
isCNF :: Form -> Bool
isCNF (Cnj xs) = all isDis xs || all isLit xs
    where isDis (Dsj xs) = all isLit xs 
          isDis _ = False
          isLit (Prop n) = True
          isLit (Neg (Prop n)) = True
          isLit _ = False
isCNF _ = False

-- The properties tested are isCNF and logical equivelance.
-- logical equivelance is a weaker property than isCNF. Logical equivalence can be achieved by any logical permutation.
testR :: Int -> Int -> IO ()
testR k n = if k == n then print (show n ++ " tests passed")
                else do
                  xs <- formGenerator 3
                  let cnf = toCNF xs
                  if isCNF cnf && equiv xs cnf then
                    do print ("pass on: " ++ show xs)
                       testR (k+1) n
                  else error ("failed test on: " ++ show xs)