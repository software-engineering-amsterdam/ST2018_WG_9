
module Lab5 where
 
import Data.List
import System.Random
import Lecture5
        
    
solveNRC :: IO [()]
solveNRC = solveAndShow exampleNRC


-- | Exercise 3
-- makes a list of sudoku 'states' that all have one value removed
eraseValues :: Node -> [Node]
eraseValues (s,cs) = map ((\x -> (x, constraints x)) . eraseS s) (filledPositions s)

-- check whether all elements in the list do not have an unique solution and
-- if the original sudoku (i.e. node) does have a unique solution
-- if this holds, then it is a minimal sudoku problem.
isMinimal :: IO ()
isMinimal = do node <- genRandomSudoku >>= genProblem
               showNode node
               let b1 = all (not . uniqueSol) (eraseValues node)
               let b2 = uniqueSol node
               print $ b1 && b2

-- Because genRandomSudoku is will always generate a minimal sudoku problem, we can change the implementation
-- to something that takes 

-- | Exercise 4



-- randomBlock (grid2sud example1)
combs :: [Position]
combs = [(a,b) | a <- [1..3], b <- [1..3]]

remove3RandomBlocks :: IO ()
remove3RandomBlocks = do -- First block
                      (sudoku, cons) <- genRandomSudoku
                      pos1 <- getRandomItem' combs
                      let removed = removePos sudoku pos1
                      -- Second block
                      let deletedList = delete' pos1 combs
                      pos2 <- getRandomItem' (delete' pos1 combs)
                      let removed2 = removePos removed pos2
                      -- Third block
                      let deletedList2 = delete' pos2 deletedList
                      pos3 <- getRandomItem' deletedList2
                      let removed3 = removePos removed2 pos3
                      problem <- genProblem (removed3, constraints removed3)
                      showNode problem

getSubGridPos :: Position -> [Position]
getSubGridPos pos = [(r,c) | r <- bl (fst pos * 3), c <- bl (snd pos * 3)]


removePos :: Sudoku -> Position -> Sudoku
removePos s pos = foldl eraseS s (getSubGridPos pos)    

delete' :: Position -> [Position] -> [Position]
delete' pos xs = [ x | x <- xs, x /= pos ]
                      
getRandomItem' :: [Position] -> IO Position
getRandomItem' [] = return (0,0)
getRandomItem' xs = do n <- getRandomInt maxi
                       return (xs !! n)
                  where maxi = length xs - 1

-- *Lab5> remove3RandomBlocks
-- +-------+-------+-------+
-- |       |       | 7   5 |
-- |  +----|--+ +--|----+  |
-- |  |    |  |9|7 | 8  |  |
-- |  |    |  | |  |    |  |
-- +-------+-------+-------+
-- | 1|9 4 |  | |  |   2|  |
-- |  +----|--+ +--|----+  |
-- |       |       |     8 |
-- |  +----|--+ +--|----+  |
-- |  |  8 |  | |  |   6|  |
-- +-------+-------+-------+
-- |  |    | 5| |  |    |6 |
-- |  |    | 3| |  |    |  |
-- |  +----|--+ +--|----+  |
-- |       |   7   |     4 |
-- +-------+-------+-------+
-- (2.35 secs, 2,176,032,320 bytes)

remove4RandomBlocks :: IO ()
remove4RandomBlocks = do -- First block
                      (sudoku, cons) <- genRandomSudoku
                      pos1 <- getRandomItem' combs
                      let removed = removePos sudoku pos1
                      -- Second block
                      let deletedList = delete' pos1 combs
                      pos2 <- getRandomItem' (delete' pos1 combs)
                      let removed2 = removePos removed pos2
                      -- Third block
                      let deletedList2 = delete' pos2 deletedList
                      pos3 <- getRandomItem' deletedList2
                      let removed3 = removePos removed2 pos3
                      -- Fourth block
                      let deletedList3 = delete' pos3 deletedList2
                      pos4 <- getRandomItem' deletedList3
                      let removed4 = removePos removed3 pos4
                      problem <- genProblem (removed4, constraints removed4)
                      showNode problem

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