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

-- Below is an attempt at a first cnf function. However, after attempting this method for a long time, I (and others) came to the conclusion
-- there were too many edge cases which made this approach almost impossible.
-- Instead I have approached the problem using truth tables (see below).
cnfIncomplete :: Form -> Form
cnfIncomplete = arrowfree # nnf # mcjs

mcjs :: Form -> Form 
mcjs (Prop x)                 = Prop x
mcjs (Neg (Prop x))           = Neg (Prop x)
mcjs (Dsj [Cnj [f1, f2], f3]) = Cnj [mcjs $ Dsj [f1, f3], mcjs $ Dsj [f2, f3]]
mcjs (Dsj [f3, Cnj [f1, f2]]) = Cnj [mcjs $ Dsj [f1, f3], mcjs $ Dsj [f2, f3]]
mcjs (Cnj fs)                 = Cnj (map mcjs fs)
mcjs (Dsj fs)                 = Dsj (map mcjs fs)

-- For the truth table approach, take all instances for which the function evaluates to false. For those instances,
-- put all the literals in a disjunction, negating them if they are false in the truth table. Then put all of that in a
-- conjunction.
-- p ^ q
-- p q f
-- 0 0 0
-- 0 1 0
-- 1 0 0
-- 1 1 1

cnf :: Form -> Form
cnf form = Cnj (map nnf negCnjs)
    where negCnjs = map Neg cnjForms
          cnjForms = map Cnj forms
          forms = map (map valToLit) vals
          vals = filter (\x -> not (evl x form)) (allVals form)

valToLit :: (Name, Bool) -> Form
valToLit (x, True) = Prop x
valToLit (x, False) = Neg (Prop x)

-- Exercise 4

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (1,n))

-- Generate a random formula, starting with p (Prop 1). p is passed into the subFormLoop which applies the subForm function
-- n times. The subForm function traverses the expression tree of the formula until it finds an atom (Prop x), then replaces it
-- with a random operator. It executes this loop a random amount of times, each time replacing all atoms with operators, building
-- a larger formula every iteration.
genFormula :: IO Form
genFormula = do
                n <- getRandomInt 6
                subFormLoop n p

-- Function which acts as a for loop. It applies subForm to the form parameter n times.
subFormLoop :: Int -> Form -> IO Form
subFormLoop 0 form = return form
subFormLoop n form = do
                        newForm <- subForm form
                        subFormLoop (n-1) newForm

subForm :: Form -> IO Form
subForm (Prop x) = do
                    n <- getRandomInt 6
                    return $ subLiterals n (Prop x)
subForm (Neg (Prop x)) = do
                        n <- getRandomInt 6
                        return $ Neg (subLiterals n (Prop x))
subForm (Neg x) = do
                    p1 <- subForm x
                    return $ Neg p1
subForm (Dsj [Prop x, Prop y]) = do
                                    n <- getRandomInt 6
                                    k <- getRandomInt 6
                                    return (Dsj [subLiterals n (Prop x), subLiterals k (Prop y)])
subForm (Dsj [x, y]) = do
                        p1 <- subForm x
                        p2 <- subForm y
                        return (Dsj [p1, p2])
subForm (Cnj [Prop x, Prop y]) = do
                                    n <- getRandomInt 6
                                    k <- getRandomInt 6
                                    return (Cnj [subLiterals n (Prop x), subLiterals k (Prop y)])
subForm (Cnj [x, y]) = do
                        p1 <- subForm x
                        p2 <- subForm y
                        return (Cnj [p1, p2])
subForm (Impl (Prop x) (Prop y)) = do
                                    n <- getRandomInt 6
                                    k <- getRandomInt 6
                                    return (Impl (subLiterals n (Prop x)) (subLiterals k (Prop y)))
subForm (Impl x y) = do
                      p1 <- subForm x
                      p2 <- subForm y
                      return (Impl p1 p2)
subForm (Equiv (Prop x) (Prop y)) = do
                                    n <- getRandomInt 6
                                    k <- getRandomInt 6
                                    return (Equiv (subLiterals n (Prop x)) (subLiterals k (Prop y)))
subForm (Equiv x y) = do
                    p1 <- subForm x
                    p2 <- subForm y
                    return (Equiv p1 p2)


subLiterals :: Int -> Form -> Form
subLiterals 1 (Prop x) = Prop x
subLiterals 2 (Prop x) = Dsj [Prop x, Prop (x + 1)]
subLiterals 3 (Prop x) = Cnj [Prop x, Prop (x + 1)]
subLiterals 4 (Prop x) = Impl (Prop x) (Prop (x + 1))
subLiterals 5 (Prop x) = Equiv (Prop x) (Prop (x + 1))
subLiterals 6 (Prop x) = Neg (Prop x)

-- cnf property
isCnf :: Form -> Bool
isCnf (Cnj fs) = all isDsj fs || all isLit fs
    where isLit (Prop x)       = True
          isLit (Neg (Prop x)) = True
          isLit _              = False
          isDsj (Dsj xs)       = all isLit xs
          isDsj _              = False
isCnf _ = False



