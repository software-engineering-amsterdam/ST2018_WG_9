module Lab3 where
 
import Data.List
import System.Random
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
-- The only way to test the implementation is by using test cases. I will use the show function
-- to convert predefined forms to a string, then feed the string into the parse function
-- and see if the result is equal to the original formula.

forms = [form1, form2, form3, form4, form5, form6, form7, form8, form9]

-- This function returns true if for all formulas in the 'forms' list, the result of
-- the 'parse' function applied to the 'show' of the formula is equal to the original formula.
testParse :: IO Bool
testParse = do
                let fs = map show forms
                let ps = concatMap parse fs
                return $ ps == forms


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

-- Exercise 4

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
subformGenerator (Prop x) = do n <- getRandomInt 5
                               return $ substituteLiterals n (Prop x)
subformGenerator (Impl a b) = do newFormA <- subformGenerator a
                                 newFormB <- subformGenerator b
                                 return $ Impl newFormA newFormB
subformGenerator (Dsj xs)   = do newForms <- mapM subformGenerator xs
                                 return $ Dsj newForms
subformGenerator (Cnj xs) = do  newForms <- mapM subformGenerator xs
                                return $ Cnj newForms
subformGenerator (Neg(x)) = do  newForm <- subformGenerator x
                                return $ Neg(newForm)     
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