module Lab5 where
 
import Data.List
import System.Random
import Lecture5
        
-- | Exercise 4
-- Runners
empty3BlockGen :: IO ()
empty3BlockGen = emptyNblockGen 3

empty4BlockGen :: IO ()
empty4BlockGen = emptyNblockGen 4

empty5BlockGen :: IO ()
empty5BlockGen = emptyNblockGen 5

-- Implementation
-- Param:   n - Amount of blocks to remove
emptyNblockGen :: Int -> IO ()
emptyNblockGen n = do (s, ctrs) <- genRandomSudoku
                      erased <- getErased n s [(a,b) | a <- [1..3], b <- [1..3]]
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


