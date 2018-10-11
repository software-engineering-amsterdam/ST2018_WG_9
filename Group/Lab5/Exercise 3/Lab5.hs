
module Lab5 where
 
    import Data.List
    import System.Random
    import Lecture5
            
    -- Exercise 1
    -- Call to solve the nrc problem    
    solveNRC :: IO [()]
    solveNRC = solveAndShow exampleNRC
    
    -- | Exercise 3
    -- Time: 30m
    -- Takes a node, and returns a list of nodes containing all unique sudokus where exactly one element
    -- of the given node's sudoku has been removed.
    eraseValues :: Node -> [Node]
    eraseValues (s,cs) = map ((\x -> (x, constraints x)) . eraseS s) (filledPositions s)

    -- Minimal property of a sudoku. Checks if all sudokus obtained by removing exactly one element from the given sudoku
    -- have more than 1 solution, and the given sudoku has exactly 1 solution.
    isMinimal :: Node -> Bool
    isMinimal node = all (not . uniqueSol) (eraseValues node) && uniqueSol node

    -- Check if a generated sudoku is minimal.
    -- Test report:
    -- +-------+-------+-------+
    -- | 1     |     9 | 6     |
    -- |  +----|--+ +--|----+  |
    -- |  |    |  |2|  |    |1 |
    -- |  |    |  | |  |    |  |
    -- +-------+-------+-------+
    -- |  |  3 |  | |7 |    |  |
    -- |  +----|--+ +--|----+  |
    -- |       |     3 | 2 8   |
    -- |  +----|--+ +--|----+  |
    -- |  |    | 8| |  |    |  |
    -- +-------+-------+-------+
    -- | 8|4   |  |5|  |    |3 |
    -- |  |    | 6| |  |   1|  |
    -- |  +----|--+ +--|----+  |
    -- |       |       |       |
    -- +-------+-------+-------+
    -- True
    checkMinimal :: IO ()
    checkMinimal = 
        do node <- genRandomSudoku >>= genProblem
           showNode node
           print (isMinimal node)
    
    
           