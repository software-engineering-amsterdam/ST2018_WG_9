module Lab3 where
 
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3


-- EXERCISE 1 --
-- 45 min
-- The defintions are checked by testing the method against forms that are proven to be the definition.



-- | contradiction
-- contradiction contraForm returns True
contradiction :: Form -> Bool
contradiction f = all (\x -> not (evl x f)) (allVals f)

-- p ^ -p is always false according to proposition logic
contraForm :: Form
contraForm = Cnj [Prop 1, Neg(Prop 2)]

-- | tautology
-- tautology tautForm returns True
tautology :: Form -> Bool
tautology f = all (`evl` f) (allVals f)

-- p v -p is always true according to proposition logic
tautForm :: Form
tautForm = Dsj [Prop 1, Neg(Prop 2)]

-- | logical entailment
-- entails entailForm1 entailForm2 returns True
-- entails entailForm2 entailForm1 returns False
entails :: Form -> Form -> Bool
entails f1 f2 = all (\x -> evl x f1 --> evl x f2) (allVals f1)

-- p ^ q
entailForm1 :: Form
entailForm1 = Cnj [Prop 1, Prop 2]

-- (p ^ q) v r
-- if entailForm1 is tue, entailForm2 is also true by definition of v
entailForm2 :: Form
entailForm2 = Dsj [entailForm1, Prop 3]

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f1 f2 = all (\x -> evl x f1 == evl x f2) (mostvals f1 f2)
    where mostvals f1 f2 | length(allVals f1) > length(allVals f2) = allVals f1
                         | otherwise = allVals f2


-- Exercise 2 --
-- 30 mins
-- This manual test, first converts some known forms to raw strings. These strings are used as input for the parser.
-- The forms that result from the parser are then compared to the initial list. If all are equal, the tests succeeds.
-- This test is still a bit sparse however, form generation could be used to broaden the input domain.

-- Test returns true on the input forms

forms :: [Form]
forms = [form1, form2, form3]

testParser :: IO Bool
testParser = do let rawForms = map show forms
                let result = concatMap parse rawForms
                return $ all (`elem` forms) result

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