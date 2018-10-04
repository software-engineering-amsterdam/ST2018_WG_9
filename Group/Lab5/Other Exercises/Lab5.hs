
module Lab5 where
 
    import Data.List
    import System.Random
    import Lecture5
            
    -- Exercise 1
    -- Call to solve the nrc problem    
    solveNRC :: IO [()]
    solveNRC = solveAndShow exampleNRC
    
    -- | Exercise 3
    -- Takes a node, and returns a list of nodes containing all unique sudokus where exactly one element
    -- of the given node's sudoku has been removed.
    eraseValues :: Node -> [Node]
    eraseValues (s,cs) = map ((\x -> (x, constraints x)) . eraseS s) (filledPositions s)

    -- Minimal property of a sudoku. Checks if all sudokus obtained by removing exactly one element from the given sudoku
    -- have more than 1 solution, and the given sudoku has exactly 1 solution.
    isMinimal :: Node -> Bool
    isMinimal node = all (not . uniqueSol) (eraseValues node) && uniqueSol node

    -- Check if a generated sudoku is minimal.
    checkMinimal :: IO ()
    checkMinimal = 
        do node <- genRandomSudoku >>= genProblem
           showNode node
           print (isMinimal node)
    
    -- EXERCISE 4, 5 --
    -- 1h
    -- The generator generates a nrc or normal sudoku dependent on the constraints defined in Lecture5.hs
    -- Currently the nrc constraints are added, therefore this generator generates nrc valid sudokus.
    -- Removing these constraints results in normal sudokus.

    -- A valid nrc sudoku with 5 empty blocks does exist: 
    -- +-------+-------+-------+
    -- |       |       | 1 7   |
    -- |       |       | 8   6 |
    -- |       |       | 9 2   |
    -- +-------+-------+-------+
    -- |   3   |       |       |
    -- |     8 |       |       |
    -- |   6   |       |       |
    -- +-------+-------+-------+
    -- |       | 7     |       |
    -- | 7 8   | 9   4 |       |
    -- |   4   | 2     |       |
    -- +-------+-------+-------+
    -- With a single solution, when running solveAndShow:
    -- +-------+-------+-------+
    -- | 8 9 5 | 6 4 2 | 1 7 3 |
    -- | 3 2 7 | 5 9 1 | 8 4 6 |
    -- | 4 1 6 | 8 7 3 | 9 2 5 |
    -- +-------+-------+-------+
    -- | 1 3 9 | 4 2 7 | 6 5 8 |
    -- | 5 7 8 | 1 6 9 | 4 3 2 |
    -- | 2 6 4 | 3 8 5 | 7 1 9 |
    -- +-------+-------+-------+
    -- | 6 5 1 | 7 3 8 | 2 9 4 |
    -- | 7 8 2 | 9 5 4 | 3 6 1 |
    -- | 9 4 3 | 2 1 6 | 5 8 7 |
    -- +-------+-------+-------+

    fiveEmptyBlocks :: Grid
    fiveEmptyBlocks =  [[0,0,0,0,0,0,1,7,0],
                        [0,0,0,0,0,0,8,0,6],
                        [0,0,0,0,0,0,9,2,0],
                        [0,3,0,0,0,0,0,0,0],
                        [0,0,8,0,0,0,0,0,0],
                        [0,6,0,0,0,0,0,0,0],
                        [0,0,0,7,0,0,0,0,0],
                        [7,8,0,9,0,4,0,0,0],
                        [0,4,0,2,0,0,0,0,0]]

    emptyBlockGen :: IO ()
    emptyBlockGen = do  (s, ctrs) <- genRandomSudoku
                        erased <- getErased 3 s [(a,b) | a <- [1..3], b <- [1..3]]
                        result <- genProblem (erased, constraints erased)
                        showNode result

    -- Params:  n - Amount of blocks to remove
    --          s - sudoku to remove blocks on
    --          poss - The bag of possible block locations to avoid duplicate removal
    -- Returns: Sudoku with n erased blocks
    getErased :: Int -> Sudoku -> [Position] -> IO Sudoku
    getErased 0 s _ = return s
    getErased n s poss = do (r, left) <- randomBlock poss
                            newSod <- eraseBlock r s
                            getErased (n - 1) newSod left
                            
    -- The position input is the position of the block
    -- The upperleft block is on (1,1) and the lower right at (3,3)
    -- Returns the sudoku given a block position, with all the elements of that block removed
    eraseBlock :: Position -> Sudoku -> IO Sudoku
    eraseBlock (rb, cb) s = do  let indices = [(r', c') | r' <- bl (rb * 3), c' <- bl (cb * 3)]
                                return (eraseSN s indices)

    -- Erases element in sudoku, for n indices
    eraseSN :: Sudoku -> [Position] -> Sudoku
    eraseSN = foldl eraseS

    -- Given a bag of positions, draw one random and return the list without the drawn element.
    randomBlock :: [Position] -> IO (Position, [Position])
    randomBlock ps = do index <- getRandomInt (length ps - 1)
                        return (ps !! index, removeItem (ps !! index) ps)
                        where   removeItem _ []                 = []
                                removeItem x (y:ys) | x == y    = removeItem x ys
                                                    | otherwise = y : removeItem x ys

    
    -- *Lab5> remove4RandomBlocks
    -- +-------+-------+-------+
    -- | 9     |       |   7   |
    -- |  +----|--+ +--|----+  |
    -- | 8|  3 |  | |  |   5|  |
    -- |  |    |  | |  |   1|9 |
    -- +-------+-------+-------+
    -- |  |  1 | 5| |  |    |  |
    -- |  +----|--+ +--|----+  |
    -- |   4 2 |       |       |
    -- |  +----|--+ +--|----+  |
    -- | 5|  7 |  |4|  |    |  |
    -- +-------+-------+-------+
    -- |  |    |  | |6 |    |  |
    -- |  |    | 4| |  |    |  |
    -- |  +----|--+ +--|----+  |
    -- |       |       |       |
    -- +-------+-------+-------+