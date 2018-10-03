module Lab3 where
    import Data.List
    import System.Random
    import Test.QuickCheck
    import Lecture3

    -- Exercise 1
    -- Time spent: 30min
    -- I checked the defintions by evaluating forms where already is proven what they should evaluate to
    form4, form5, form6, form7 :: Form
    form4 = Dsj [Neg p, p] -- Tautologie
    form5 = Cnj [p, Neg p] -- Contradiction 
    form6 = Cnj [p,q] -- Satisfiable    
    form7 = Cnj [q,p] -- Satisfiable    

    -- Implementation of contradiction,
    -- will return true if all of the valuations of the form result into false
    contradiction :: Form -> Bool
    contradiction = not . satisfiable

    -- Implementation of tautology, 
    -- will return true if all of the valuations of the form result into true
    tautology :: Form -> Bool
    tautology form = all (`evl` form) (allVals form) 

     -- | logical entailment 
    entails :: Form -> Form -> Bool
    entails form1 form = all (\val -> evl val form1 --> evl val form2) (allVals form) 

    -- | logical equivalence
    equiv :: Form -> Form -> Bool
    equiv form1 form2 = entails form1 form2 && entails form2 form1

    -- Exercise 2
    -- Time spent: 45min
    -- The call of parseTest on forms returns: true
    -- I am showing each for to get a string and parse this string to get a form.
    -- Then I can compare the parsed string to the original form
    forms = [form5, form6, form7]
    
    parseTest :: [Form] -> Bool
    parseTest = all (\form -> equiv ((head . parse . show) form) form)

    -- Exercise 3
    -- Time spent: 60min
    -- I used the truthtable approach. So First we find all the valuations that makes the form false.
    -- Then we take the conjunction of all negations of the conjunctions of the literals in the valuation where we also applied the predefined nnf function.
    
    convertToCNF :: Form -> Form
    convertToCNF form   | tautology form = Cnj [p, p]
                        | otherwise = Cnj (map valToForm' (filter (\val -> not (evl val form)) (allVals form)))

    valToForm' :: Valuation -> Form
    valToForm' val = valToForm val []

    valToForm :: Valuation -> [Form] -> Form
    valToForm [] acc = nnf (Neg(Cnj acc))
    valToForm (x:xs) acc    | snd x = valToForm xs (Prop (fst x):acc)
                            | otherwise = valToForm xs (Neg (Prop (fst x)):acc) 

    -- Exercise 4
    -- Time spent: 90min

    
    isCNF :: Form -> Bool
    isCNF (Cnj x) = all isDsjOrLit x
    isCNF _ = False

    isDsjOrLit :: Form -> Bool
    isDsjOrLit (Prop x) = True
    isDsjOrLit (Dsj x) = all isDsj' x
        where   isDsj' (Prop n) = True
                isDsj' (Neg (Prop n)) = True
                isDsj' _ = False
                
    isDsjOrLit _ = False
    noCnj :: Form -> Bool
    noCnj (Cnj x)   = True
    noCnj _             = False

    getRandomInt :: Int -> IO Int
    getRandomInt n = getStdRandom (randomR (0,n))
    
    formGenerator :: IO Form
    formGenerator = do
                    xs <- mapM (subFormGenerator . Prop) [1..10]
                    return $ Cnj xs
    subFormGenerator :: Form -> IO Form
    subFormGenerator (Prop n) = do 
            randomNumber <- getRandomInt 5
            return (possibleLiteral randomNumber (Prop n))
    
    possibleLiteral :: Int -> Form -> Form
    possibleLiteral 0 (Prop n) = Prop n 
    possibleLiteral 1 (Prop n) = Dsj[Prop n, Prop (n + 1)]
    possibleLiteral 2 (Prop n) = Cnj[Prop n, Prop (n + 1)]
    possibleLiteral 3 (Prop n) = Impl (Prop n) (Prop n)
    possibleLiteral 4 (Prop n) = Equiv (Prop n) (Prop n)
    possibleLiteral 5 (Prop n) = Neg(Prop n)
    

    testRunner :: Int -> Int -> IO ()
    testRunner k n = if k == n then print (show n ++ " tests passed")
                else do
                  form <- formGenerator
                  if isCNF (convertToCNF form) then
                    do print ("pass on: " ++ show form)
                       testRunner (k+1) n
                  else error ("failed test on: " ++ show form)

    