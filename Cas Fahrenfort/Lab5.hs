module Lab5 where

import Data.List
import System.Random
import Control.Monad
import Lecture5Ass1
import Lecture5Ass2
import Lecture5Ass3

-- Exercise 1 --
-- import Lecture5Ass1

-- Formalized constraint: 3x3 subgrid with top-left corner (r,c) should yield an injective function
-- for (r,c) ∈ { (2,2), (2,6), (6,2), (6,6) }

-- For bonus points, the showGrid function was altered to also display the NRC subgrids.
exampleNRC :: Lecture5Ass1.Grid
exampleNRC = [[0,0,0,3,0,0,0,0,0],
             [0,0,0,7,0,0,3,0,0],
             [2,0,0,0,0,0,0,0,8],
             [0,0,6,0,0,5,0,0,0],
             [0,9,1,6,0,0,0,0,0],
             [3,0,0,0,7,1,2,0,0],
             [0,0,0,0,0,0,0,3,1],
             [0,8,0,0,4,0,0,0,0],
             [0,0,2,0,0,0,0,0,0]]

-- Running this function executes the solveAndShow function with the example NRC sudoku.
-- Refactoring the function to be able to solve NRC sudokus consisted of the following steps:
-- * Define extra 'nrcBlocks' to consider for the sudoku problem
-- * Add new 'nrcbl' function to concat list of nrcBlocks function
-- * Define 'nrcGrid' which lists all values from an nrcGrid
-- * 'freeInNRCgrid' which lists all possible values in an nrcGrid
-- * 'nrcSubGridInjective' which checks if an nrcGrid is injective
-- * 'samenrcblock' which checks if two coordinates are within the same nrcBlock
-- * Change the 'prune' function to consider the samenrcblock case
solveExampleNRC :: IO [()]
solveExampleNRC = Lecture5Ass1.solveAndShow exampleNRC

-- Exercise 2
-- import Lecture5Ass2

-- Added nrcBlockConstrnts
-- This way it is easier to add new constraints. code is compacter & mooier 
-- -> gaat ten koste van iets (iets gebeurt niet waardoor het langzamer wordt) -> minder efficient, looptijd -> 

-- Exercise 3
-- import Lecture5Ass3

-- Takes a node, and returns a list of nodes containing all unique sudokus where exactly one element
-- of the given node's sudoku has been removed.
eraseValues :: Lecture5Ass3.Node -> [Lecture5Ass3.Node]
eraseValues (s,cs) = map ((\x -> (x, Lecture5Ass3.constraints x)) . Lecture5Ass3.eraseS s) (Lecture5Ass3.filledPositions s)

-- Minimal property of a sudoku. Checks if all sudokus obtained by removing exactly one element from the given sudoku
-- have more than 1 solution, and the given sudoku has exactly 1 solution.
isMinimal :: Lecture5Ass3.Node -> Bool
isMinimal node = all (not . Lecture5Ass3.uniqueSol) (eraseValues node) && Lecture5Ass3.uniqueSol node

-- Check if a generated sudoku is minimal.
checkMinimal :: IO ()
checkMinimal = 
    do node <- Lecture5Ass3.genRandomSudoku >>= Lecture5Ass3.genProblem
       Lecture5Ass3.showNode node
       print (isMinimal node)

-- Exercise 4
-- import Lecture5Ass3

getRandomInt' :: Int -> Int -> IO Int
getRandomInt' n r = getStdRandom (randomR (n,r))

randomBlocks :: IO [(Int,Int)]
randomBlocks = 
        do  i <- getRandomInt' 0 2
            j <- getRandomInt' 0 8
            k <- getRandomInt' 3 5
            l <- getRandomInt' 0 8
            m <- getRandomInt' 6 8
            n <- getRandomInt' 0 8
            let is = [(i+1,j+1),(k+1,l+1),(m+1,n+1)]
            if length (nub is) == 3
            then return is
            else randomBlocks

eraseBlock :: Lecture5Ass3.Node -> (Int,Int) -> Lecture5Ass3.Node
eraseBlock (s,cs) (i,j) = (newSudoku, Lecture5Ass3.constraints newSudoku)
    where newSudoku = foldl Lecture5Ass3.eraseS s indices
          indices = [(r, c) | r <- Lecture5Ass3.bl i, c <- Lecture5Ass3.bl j]

genErasedBlocksSudoku :: IO Lecture5Ass3.Node
genErasedBlocksSudoku = do
        node <- Lecture5Ass3.genRandomSudoku
        erasedBlocksNode <- foldl eraseBlock node <$> randomBlocks
        Lecture5Ass3.genProblem erasedBlocksNode

-- Exercise 5
-- import Lecture5Ass1
-- The random sudoku generator uses the sudoku solver. In exercise 1 we already implemented NRC sudoku constraints.
-- Generating a random sudoku with these constraints in place generates a valid NRC sudoku.

genNRC :: IO ()
genNRC = 
    do x <- Lecture5Ass1.genRandomSudoku
       y <- Lecture5Ass1.genProblem x
       Lecture5Ass1.showNode x
       Lecture5Ass1.showNode y

