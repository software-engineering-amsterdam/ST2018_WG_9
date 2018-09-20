
module Lab3 where
 
import Data.List
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

-- lexer :: String -> [Token]
-- lexer [] = []
-- lexer (c:cs) | isSpace c = lexer cs            -- ignore spaces
--              | isDigit c = lexNum (c:cs)       -- literal
-- lexer ('(':cs) = TokenOP : lexer cs            -- open parenthesis  (
-- lexer (')':cs) = TokenCP : lexer cs            -- close parenthesis )
-- lexer ('*':cs) = TokenCnj : lexer cs           -- Conjunction ^ = *
-- lexer ('+':cs) = TokenDsj : lexer cs           -- Disjuncton  v = +
-- lexer ('-':cs) = TokenNeg : lexer cs           -- Negation - 
-- lexer ('=':'=':'>':cs) = TokenImpl : lexer cs  -- implication ==>
-- lexer ('<':'=':'>':cs) = TokenEquiv : lexer cs -- Eqivalence <=>
-- lexer (x:_) = error ("unknown token: " ++ [x]) 

-- The show instance converts the datatype Form to the correct representation for the lexer

-- | Exercise 2 - Time: 30 min
forms :: [Form]
forms = [form1, form2, form3]

testParser :: IO Bool
testParser = do let input = map show forms
                let result = concatMap parse input
                return $ all (`elem` forms) result

-- Note that forms can be of any form of input, the function assesses if the parse result was done correctly in relation to
-- the input by the use of `elem`

-- | Exercise 3 - Time: 
-- Step 1: use the equivalence between p→q and ¬p∨q to get rid of → symbols
-- Step 2: use the equivalence of p↔q and (¬p∨q)∧(p∨¬q) to get rid of ↔ symbols
-- Step 3: p↔q and (p∧q)∨(¬p∧¬q) is also equivalent
-- Step 4: check for conjunction of disjunction of literals (p v q) ^ (-p v q)

cnf :: Form -> Form
cnf = flatdsj . flat . cnf' . nnf . arrowfree
    where cnf' :: Form -> Form
          cnf' (Cnj [f1,f2])       = Cnj [cnf' f1, cnf' f2]
          cnf' (Dsj [f1,f2])       = dist (head [cnf' f1, cnf' f2]) ([cnf' f1, cnf' f2] !! 1) 
          cnf' expr                = expr

          dist (Cnj [e11, e12]) e2 = Cnj [e11 `dist` e2, e12 `dist` e2] 
          dist e1 (Cnj [e21,e22])  = Cnj [e1 `dist` e21, e1 `dist` e22]
          dist e1 e2               = Dsj [e1,e2]

flat :: Form -> Form
flat (Dsj [Dsj f1, Dsj f2])       = Dsj ((map flat f1) ++ (map flat f2))
flat (Dsj [Dsj f1, Prop x])       = Dsj ((map flat f1) ++ [Prop x])
flat (Dsj [Dsj f1, Neg (Prop x)]) = Dsj ((map flat f1) ++ [Neg (Prop x)])
flat (Cnj f)                      = Cnj (map flat f)
flat expr = expr

flatdsj :: Form -> Form
flatdsj (Cnj [Cnj f1, Cnj f2])   = Cnj ((map flatdsj f1) ++ (map flatdsj f2))
flatdsj (Cnj [Cnj f1, Neg (Prop x)])   = Cnj ((map flatdsj f1) ++ [Neg (Prop x)])
flatdsj (Cnj [Cnj f1, Prop x])         = Cnj ((map flatdsj f1) ++ [Prop x])
--flatdsj (Cnj f1)                       = Cnj (map flatdsj f1)
flatdsj (Cnj list)                     = Cnj (map flatdsj [f | f <- list])
flatdsj expr                           = expr
    
-- (((-a v b a) ^ (-a v b -b)) ^ ((-a v b -b) ^ (-a v b a)) ((b v -a a) ^ (b v -a -b))((b v -a -b) ^ (b v -a a)))
-- ((((-a v b a) ^ (-a v b -b)) ^ ((-a v b -b) ^ (-a v b a))) ^ (((b v -a a) ^ (b v -a-b)) ^ ((b v -a -b) ^ (b v -a a))))

justHelper :: Maybe [a] -> [a]
justHelper (Just x) = x
justHelper Nothing = []
