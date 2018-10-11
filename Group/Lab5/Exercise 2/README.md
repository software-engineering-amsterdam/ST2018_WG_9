# Exercise 2 - Time: 1h45m
## Changes made in regards to Exercise 1
``` haskell
type Position = (Row,Column)
type Constrnt = [[Position]]

-- New freeAtPos
freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
freeAtPos' s (r,c) xs = let ys = filter (elem (r,c)) xs 
                        in foldl1 intersect (map ((values \\) . map s) ys)

-- Old freeAtPos
-- From all the possible values, substract the one from constraint, output all values
freeInSeq :: [Value] -> [Value]
freeInSeq seq = values \\ seq 

freeInRow :: Sudoku -> Row -> [Value]
freeInRow s r = freeInSeq [ s (r,i) | i <- positions  ]

freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn s c = freeInSeq [ s (i,c) | i <- positions ]

freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid s (r,c) = freeInSeq (subGrid s (r,c))

freeInNRCgrid :: Sudoku -> (Row, Column) -> [Value]
freeInNRCgrid s (r,c) = freeInSeq (nrcSubGrid s (r,c))

freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos s (r,c) = (freeInRow s r) `intersect` (freeInColumn s c) `intersect` (freeInSubgrid s (r,c)) `intersect` (freeInNRCgrid s (r,c))

```

If we compare the implementation of `freeAtPos` with `freeAtPos'`, we see multiple recurring elements that have been made easier

* `freeInSeq`'s difference function moved to the function of the `map`  
* `intersect` moved to the function of the `foldl1`  
* List comprehension that denote the constraint as an argument of `freeAtPos'`  

This means that, where `freeAtPos'` is called, the constraints have to be defined there. This is where the following come in:
``` haskell
rowConstrnt, columnConstrnt, blockConstrnt :: [[Position]]
rowConstrnt    = [[(r,c)| c <- values ] | r <- values ]
columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
blockConstrnt  = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
```
Note that `values` in `freeInSeq` is now added as a predicate in the list here, since `freeAtPos'` will apply the difference.  

The only thing left to do is to replace the original `freeAtPos` function call with `freeAtPos'`, where the third argument are the above constraints as a concatanated list by use of `(++)`
``` haskell
constraints :: Sudoku -> [Constraint] 
constraints s = sortBy length3rd [(r,c, freeAtPos' s (r,c) constrnts) | (r,c) <- openPositions s ]
              where constrnts = rowConstrnt ++ columnConstrnt ++ blockConstrnt ++ blockNrcConstrnt
```
Note the addition of a constraint `blockNrcConstrnt`, that defined the constrnt for NRC's subgrids
``` haskell
blockNrcConstrnt :: [[Position]]
blockNrcConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- nrcBlocks, b2 <- nrcBlocks ]
```

## Comments on extendability and efficiency.
The original implementation of `freeAtPos` was more efficient when compared to `freeAtPos'`.  `freeAtPos'` uses an extra `map` that adds an O(n) to the runtime of the algorithm. Additionally, `filter (elem (r,c)) xs` was originally only present in `subGrid` and `nrcSubGrid`, now it aslo gets performed on the values of the other constraints.

``` haskell 
subGrid :: Sudoku -> (Row,Column) -> [Value]
subGrid s (r,c) = [ s (r',c') | r' <- bl r, c' <- bl c ]

nrcSubGrid :: Sudoku -> (Row, Column) -> [Value]
nrcSubGrid s (r,c) = [s (r',c') | r' <- nrcbl r, c' <- nrcbl c ]
```
Where bl and nrcbl are defined as follows
``` haskell
bl :: Int -> [Int]
bl x = concat $ filter (elem x) blocks 

nrcbl :: Int -> [Int]
nrcbl x = concat $ filter (elem x) nrcBlocks
```
Essentially, the trade-off is between readability and conciseness of the code and performance.   

The refactor is not complete yet since the `prune` and `nrcSubGridInjective` functions still uses `nrcSubGrid`. We will address this if we have additional time. 
