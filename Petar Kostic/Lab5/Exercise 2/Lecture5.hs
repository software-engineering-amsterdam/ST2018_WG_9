
module Lecture5

where 

import Data.List
import System.Random

type Row    = Int 
type Column = Int 
type Value  = Int
type Grid   = [[Value]]

positions, values :: [Int]
positions = [1..9]
values    = [1..9] 

-- In order to refactor the code such that the formulation of constraints 
-- becomes more uniform, the following definitions were proposed:
type Position = (Row,Column)
type Constrnt = [[Position]]



-- Explicitly provide the blocks that we are considering
-- for the sudoku problem
blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

nrcBlocks :: [[Int]]
nrcBlocks = [[2..4],[6..8]]

-- Note that a zero is encoded as a value that is not
-- yet defined
showVal :: Value -> String
showVal 0 = " "
showVal d = show d

-- Define my own show function to show the new subgrids
showRow :: [Value] -> IO()
showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
 do  putChar '|'         ; putChar ' '
     putStr (showVal a1) ; putChar ' '
     putStr (showVal a2) ; putChar ' '
     putStr (showVal a3) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a4) ; putChar ' '
     putStr (showVal a5) ; putChar ' '
     putStr (showVal a6) ; putChar ' '
     putChar '|'         ; putChar ' '
     putStr (showVal a7) ; putChar ' '
     putStr (showVal a8) ; putChar ' '
     putStr (showVal a9) ; putChar ' '
     putChar '|'         ; putChar '\n'

showRow2 :: IO()
showRow2 = putStrLn "|  +----|--+ +--|----+  |"

showRow3 :: [Value] -> IO()
showRow3 [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
    do putChar '|'; putChar ' '
       putStr (showVal a1) ; putChar '|'
       putStr (showVal a2) ; putChar ' '
       putStr (showVal a3) ; putChar ' '
       putChar '|' ; putChar ' ';
       putStr (showVal a4) ; putChar '|'
       putStr (showVal a5) ; putChar '|'
       putStr (showVal a6) ; putChar ' '
       putChar '|' ; putChar ' '
       putStr (showVal a7) ; putChar ' '
       putStr (showVal a8) ; putChar '|'
       putStr (showVal a9) ; putChar ' '; putChar '|';
       putChar '\n'

showGrid :: Grid -> IO()
showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
 do putStrLn "+-------+-------+-------+"
    showRow as; showRow2; showRow3 bs; showRow3 cs
    putStrLn "+-------+-------+-------+"
    showRow3 ds; showRow2; showRow es; showRow2; showRow3 fs
    putStrLn "+-------+-------+-------+"
    showRow3 gs; showRow3 hs; showRow2; showRow is
    putStrLn "+-------+-------+-------+"

-- Sudoku here is a function that given that cell, return the value
type Sudoku = (Row,Column) -> Value

sud2grid :: Sudoku -> Grid
sud2grid s = 
  [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ] 

grid2sud :: Grid -> Sudoku
grid2sud gr = \ (r,c) -> pos gr (r,c) 
  where 
  pos :: [[a]] -> (Row,Column) -> a 
  pos gr (r,c) = (gr !! (r-1)) !! (c-1)

showSudoku :: Sudoku -> IO()
showSudoku = showGrid . sud2grid

-- All the elements that are filtered by this conditions are concatenated in one integer list
bl :: Int -> [Int]
bl x = concat $ filter (elem x) blocks 

nrcbl :: Int -> [Int]
nrcbl x = concat $ filter (elem x) nrcBlocks

-- Used to define the subgrid by finding the rows and the colomns that belong to r respectively to c
subGrid :: Sudoku -> (Row,Column) -> [Value]
subGrid s (r,c) = [ s (r',c') | r' <- bl r, c' <- bl c ]

nrcSubGrid :: Sudoku -> (Row, Column) -> [Value]
nrcSubGrid s (r,c) = [s (r',c') | r' <- nrcbl r, c' <- nrcbl c ]

-- Takes a sudoku, position and new Constrnt 
freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s (r,c) xs = let ys = filter (elem (r,c)) xs 
                         in foldl1 intersect (map ((values \\) . map s) ys)

injective :: Eq a => [a] -> Bool
injective xs = nub xs == xs

rowInjective :: Sudoku -> Row -> Bool
rowInjective s r = injective vs where 
   vs = filter (/= 0) [ s (r,i) | i <- positions ]

colInjective :: Sudoku -> Column -> Bool
colInjective s c = injective vs where 
   vs = filter (/= 0) [ s (i,c) | i <- positions ]

subgridInjective :: Sudoku -> (Row,Column) -> Bool
subgridInjective s (r,c) = injective vs 
                         where vs = filter (/= 0) (subGrid s (r,c))

nrcSubGridInjective :: Sudoku -> (Row, Column) -> Bool
nrcSubGridInjective s (r,c) = injective vs
                            where vs = filter (/= 0) (nrcSubGrid s (r,c))

-- Should hold across the whole sudoku
consistent :: Sudoku -> Bool
consistent s = and $  [ rowInjective s r |  r <- positions ]
                   ++ [ colInjective s c |  c <- positions ]
                   ++ [ subgridInjective s (r,c)    | r <- [1,4,7], c <- [1,4,7]]
                   ++ [ nrcSubGridInjective s (r,c) | r <- [2,6], c <- [2,6]]

extend :: Sudoku -> ((Row,Column),Value) -> Sudoku
extend = update

-- An intermediary solution is updated with a possible solution with a possible
-- free value.
update :: Eq a => (a -> b) -> (a,b) -> a -> b 
update f (y,z) x = if x == y then z else f x 

-- What kind of values I cannot have in this row and column
type Constraint = (Row,Column,[Value])

type Node = (Sudoku,[Constraint])

showNode :: Node -> IO()
showNode = showSudoku . fst

solved  :: Node -> Bool
solved = null . snd

extendNode :: Node -> Constraint -> [Node]
extendNode (s,constraints) (r,c,vs) = [(extend s ((r,c),v), sortBy length3rd $ prune (r,c,v) constraints) | v <- vs ]

-- If one tree branch is the same configuration, remove it
prune :: (Row,Column,Value) -> [Constraint] -> [Constraint]
prune _ []                                               = []
prune (r,c,v) ((x,y,zs):rest) | r == x                   = (x,y,zs\\[v]) : prune (r,c,v) rest
                              | c == y                   = (x,y,zs\\[v]) : prune (r,c,v) rest
                              | sameblock (r,c) (x,y)    = (x,y,zs\\[v]) : prune (r,c,v) rest
                              | sameblockNRC (r,c) (x,y) = (x,y,zs\\[v]) : prune (r,c,v) rest
                              | otherwise                = (x,y,zs)      : prune (r,c,v) rest

sameblock :: (Row,Column) -> (Row,Column) -> Bool
sameblock (r,c) (x,y) = bl r == bl x && bl c == bl y

sameblockNRC :: (Row, Column) -> (Row,Column) -> Bool
sameblockNRC (r,c) (x,y) = nrcbl r == nrcbl x && nrcbl c == nrcbl y

initNode :: Grid -> [Node]
initNode gr = let s = grid2sud gr in 
              if (not . consistent) s then [] 
              else [(s, constraints s)]

-- Everything that's zero
openPositions :: Sudoku -> [(Row,Column)]
openPositions s = [ (r,c) | r <- positions, c <- positions, s (r,c) == 0 ]

-- Compares the list of values in two constraints 
-- Returns a string that encodes the semantics of < and > and in what relationship they are to eachother
length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
length3rd (_,_,zs) (_,_,zs') = compare (length zs) (length zs')

rowConstrnt, columnConstrnt, blockConstrnt, blockNrcConstrnt :: [[Position]]
rowConstrnt      = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt   = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt    = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks,    b2 <- blocks ]
blockNrcConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- nrcBlocks, b2 <- nrcBlocks ]

-- Returns a list at a row and column that you are constrained to choose from
constraints :: Sudoku -> [Constraint] 
constraints s = sortBy length3rd [(r,c, freeAtPos' s (r,c) constrnts) | (r,c) <- openPositions s ]
              where constrnts = rowConstrnt ++ columnConstrnt ++ blockConstrnt ++ blockNrcConstrnt


-- TREE RELATED
data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

exmple1 = T 1 [T 2 [], T 3 []]
exmple2 = T 0 [exmple1,exmple1,exmple1]

grow :: (node -> [node]) -> node -> Tree node 
grow step seed = T seed (map (grow step) (step seed))

count :: Tree a -> Int 
count (T _ ts) = 1 + sum (map count ts)

takeT :: Int -> Tree a -> Tree a
takeT 0 (T x _) = T x []
takeT n (T x ts) = T x $ map (takeT (n-1)) ts

-- DFS implementation
search :: (node -> [node]) -> (node -> Bool) -> [node] -> [node]
search children goal []                 = []
search children goal (x:xs) | goal x    = x : search children goal xs
                            | otherwise = search children goal (children x ++ xs)

solveNs :: [Node] -> [Node]
solveNs = search succNode solved 

succNode :: Node -> [Node]
succNode (s,[]) = []
succNode (s,p:ps) = extendNode (s,ps) p 

solveAndShow :: Grid -> IO[()]
solveAndShow gr = solveShowNs (initNode gr)

solveShowNs :: [Node] -> IO[()]
solveShowNs = traverse showNode . solveNs

example1 :: Grid
example1 = [[5,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example2 :: Grid
example2 = [[0,3,0,0,7,0,0,0,0],
            [6,0,0,1,9,5,0,0,0],
            [0,9,8,0,0,0,0,6,0],
            [8,0,0,0,6,0,0,0,3],
            [4,0,0,8,0,3,0,0,1],
            [7,0,0,0,2,0,0,0,6],
            [0,6,0,0,0,0,2,8,0],
            [0,0,0,4,1,9,0,0,5],
            [0,0,0,0,8,0,0,7,9]]

example3 :: Grid
example3 = [[1,0,0,0,3,0,5,0,4],
            [0,0,0,0,0,0,0,0,3],
            [0,0,2,0,0,5,0,9,8], 
            [0,0,9,0,0,0,0,3,0],
            [2,0,0,0,0,0,0,0,7],
            [8,0,3,0,9,1,0,6,0],
            [0,5,1,4,7,0,0,0,0],
            [0,0,0,3,0,0,0,0,0],
            [0,4,0,0,0,9,7,0,0]]

example4 :: Grid
example4 = [[1,2,3,4,5,6,7,8,9],
            [2,0,0,0,0,0,0,0,0],
            [3,0,0,0,0,0,0,0,0],
            [4,0,0,0,0,0,0,0,0],
            [5,0,0,0,0,0,0,0,0],
            [6,0,0,0,0,0,0,0,0],
            [7,0,0,0,0,0,0,0,0],
            [8,0,0,0,0,0,0,0,0],
            [9,0,0,0,0,0,0,0,0]]

example5 :: Grid
example5 = [[1,0,0,0,0,0,0,0,0],
            [0,2,0,0,0,0,0,0,0],
            [0,0,3,0,0,0,0,0,0],
            [0,0,0,4,0,0,0,0,0],
            [0,0,0,0,5,0,0,0,0],
            [0,0,0,0,0,6,0,0,0],
            [0,0,0,0,0,0,7,0,0],
            [0,0,0,0,0,0,0,8,0],
            [0,0,0,0,0,0,0,0,9]]

exampleNRC :: Grid
exampleNRC = [[0,0,0,3,0,0,0,0,0],
              [0,0,0,7,0,0,3,0,0],
              [2,0,0,0,0,0,0,0,8],
              [0,0,6,0,0,5,0,0,0],
              [0,9,1,6,0,0,0,0,0],
              [3,0,0,0,7,1,2,0,0],
              [0,0,0,0,0,0,0,3,1],
              [0,8,0,0,4,0,0,0,0],
              [0,0,2,0,0,0,0,0,0]]

emptyN :: Node
emptyN = (const 0,constraints (const 0))

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomItem :: [a] -> IO [a]
getRandomItem [] = return []
getRandomItem xs = do n <- getRandomInt maxi
                      return [xs !! n]
                   where maxi = length xs - 1

randomize :: Eq a => [a] -> IO [a]
randomize xs = do y <- getRandomItem xs 
                  if null y 
                  then return []
                  else do ys <- randomize (xs\\y)
                          return (head y:ys)

sameLen :: Constraint -> Constraint -> Bool
sameLen (_,_,xs) (_,_,ys) = length xs == length ys

getRandomCnstr :: [Constraint] -> IO [Constraint]
getRandomCnstr cs = getRandomItem (f cs) 
  where f [] = []
        f (x:xs) = takeWhile (sameLen x) (x:xs)

rsuccNode :: Node -> IO [Node]
rsuccNode (s,cs) = do xs <- getRandomCnstr cs
                      if null xs 
                      then return []
                      else return (extendNode (s,cs\\xs) (head xs))

rsolveNs :: [Node] -> IO [Node]
rsolveNs ns = rsearch rsuccNode solved (return ns)

rsearch :: (node -> IO [node]) -> (node -> Bool) -> IO [node] -> IO [node]
rsearch succ goal ionodes = 
  do xs <- ionodes 
     if null xs 
     then return []
     else if goal (head xs) 
          then return [head xs]
          else do ys <- rsearch succ goal (succ (head xs))
                  if (not . null) ys 
                  then return [head ys]
                  else if null (tail xs) 
                       then return []
                       else rsearch succ goal (return $ tail xs)

genRandomSudoku :: IO Node
genRandomSudoku = do [r] <- rsolveNs [emptyN]
                     return r

randomS = genRandomSudoku >>= showNode

uniqueSol :: Node -> Bool
uniqueSol node = singleton (solveNs [node]) where 
  singleton [] = False
  singleton [x] = True
  singleton (x:y:zs) = False

eraseS :: Sudoku -> (Row,Column) -> Sudoku
eraseS s (r,c) (x,y) | (r,c) == (x,y) = 0
                     | otherwise      = s (x,y)

eraseN :: Node -> (Row,Column) -> Node
eraseN n (r,c) = (s, constraints s) 
  where s = eraseS (fst n) (r,c) 

minimalize :: Node -> [(Row,Column)] -> Node
minimalize n [] = n
minimalize n ((r,c):rcs) | uniqueSol n' = minimalize n' rcs
                         | otherwise    = minimalize n  rcs
  where n' = eraseN n (r,c)

filledPositions :: Sudoku -> [(Row,Column)]
filledPositions s = [ (r,c) | r <- positions, c <- positions, s (r,c) /= 0 ]

genProblem :: Node -> IO Node
genProblem n = do ys <- randomize xs
                  return (minimalize n ys)
   where xs = filledPositions (fst n)

main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblem r
          showNode s

