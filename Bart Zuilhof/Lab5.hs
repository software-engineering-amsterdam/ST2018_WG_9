module Lab5 where
import Data.List
import System.Random
import Lecture5Exercise2

-- Exercise 1
-- Time spent: 90min
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

solveNRC :: IO [()]
solveNRC = solveAndShow exampleNRC
              
              
-- Solution
-- +-------+-------+-------+
-- | 4 7 8 | 3 9 2 | 6 1 5 |
-- |  +----|--+ +--|----+  |
-- | 6|1 9 | 7|5|8 | 3 2|4 |
-- | 2|3 5 | 4|1|6 | 9 7|8 |
-- +-------+-------+-------+
-- | 7|2 6 | 8|3|5 | 1 4|9 |
-- |  +----|--+ +--|----+  |
-- | 8 9 1 | 6 2 4 | 7 5 3 |
-- |  +----|--+ +--|----+  |
-- | 3|5 4 | 9|7|1 | 2 8|6 |
-- +-------+-------+-------+
-- | 5|6 7 | 2|8|9 | 4 3|1 |
-- | 9|8 3 | 1|4|7 | 5 6|2 |
-- |  +----|--+ +--|----+  |
-- | 1 4 2 | 5 6 3 | 8 9 7 |
-- +-------+-------+-------+


-- Exercise 3
-- Time spent: 30min
isMinimal :: Node -> Bool
isMinimal (sudoku, _) = all (\x-> (not. uniqueSol) (x, constraints x)) (map (eraseS sudoku) (filledPositions sudoku))

minimalTest :: IO ()
minimalTest = do    (solution, _) <- genRandomSudoku
                    blockList <- randomList 100
                    let threeBlocks = take 3 (nub blockList)
                    erased <- removeBlock solution (threeBlocks !! 0)
                    erased <- removeBlock erased (threeBlocks !! 1)
                    erased <- removeBlock erased (threeBlocks !! 2)
                    node <- genProblem (erased, constraints erased)
                    showNode node

-- We can test this 

genProblemThreeEmpty :: Node -> IO Node
genProblemThreeEmpty n = do 
    ys <- randomize xs
    return (minimalize n ys)
    where xs = filledPositions (fst n)
        
removeBlock :: Sudoku -> (Int, Int) -> IO Sudoku
removeBlock s (col, row) = do
                let positions = [(r,c) | r <- bl (row * 3), c <- bl (col * 3) ]
                return (removeIndices s positions)
                

removeIndices :: Sudoku -> [(Int, Int)] -> Sudoku
removeIndices = foldl eraseS 

randomList:: Int -> IO [(Int,Int)]
randomList 0 = return []
randomList n = do
    r <- getStdRandom (randomR (1,3))
    c <- getStdRandom (randomR (1,3))
    rs <- randomList (n - 1)
    return ((r,c):rs)

-- Exercise 5
-- Time spent 

nrcGen :: IO()
nrcGen = do 
    sudo <- genRandomSudoku
    prob <- genProblem sudo
    showNode sudo
    showNode prob