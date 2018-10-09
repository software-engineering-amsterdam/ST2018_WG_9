# Exercise 1 - Time: 1h30m (mostly spent on learning the implementation)

``` haskell
+---------+---------+---------+
|         | 3       |         |
|   +-----|--+   +--|-----+   |
|   |     | 7|   |  | 3   |   |
| 2 |     |  |   |  |     | 8 |
+---------+---------+---------+
|   |   6 |  |   |5 |     |   |
|   +-----|--+   +--|-----+   |
|    9  1 | 6       |         |
|   +-----|--+   +--|-----+   |
| 3 |     |  | 7 |1 | 2   |   |
+---------+---------+---------+
|   |     |  |   |  |    3| 1 |
|   |8    |  | 4 |  |     |   |
|   +-----|--+   +--|-----+   |
|       2 |         |         |
+---------+---------+---------+
```
Add this problem to `Lecture5.hs` codes as
``` haskell
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
```

## Solution to the above puzzle  
Note that we also defined our own show function that is roughly similar to the one above
``` haskell
*Lecture5> solveAndShow exampleNRC
+-------+-------+-------+
| 4 7 8 | 3 9 2 | 6 1 5 |
|  +----|--+ +--|----+  |
| 6|1 9 | 7|5|8 | 3 2|4 |
| 2|3 5 | 4|1|6 | 9 7|8 |
+-------+-------+-------+
| 7|2 6 | 8|3|5 | 1 4|9 |
|  +----|--+ +--|----+  |
| 8 9 1 | 6 2 4 | 7 5 3 |
|  +----|--+ +--|----+  |
| 3|5 4 | 9|7|1 | 2 8|6 |
+-------+-------+-------+
| 5|6 7 | 2|8|9 | 4 3|1 |
| 9|8 3 | 1|4|7 | 5 6|2 |
|  +----|--+ +--|----+  |
| 1 4 2 | 5 6 3 | 8 9 7 |
+-------+-------+-------+
```

## Changes to Lecture5.hs and explanation
Explicitly provide the extra blocks that we are considering for the sudoku problem  
``` haskell
blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

nrcBlocks :: [[Int]]
nrcBlocks = [[2..4],[6..8]]
```
Add a new `bl` function that to concatenate a list of our own `nrcBlocks` function

``` haskell
bl :: Int -> [Int]
bl x = concat $ filter (elem x) blocks 

nrcbl :: Int -> [Int]
nrcbl x = concat $ filter (elem x) nrcBlocks
```

The original `subGrid` function found rows and columns that that belong to `r` respectively to `c`. Now define our own version of it for NRC relates subgrids
``` haskell
subGrid :: Sudoku -> (Row,Column) -> [Value]
subGrid s (r,c) = [ s (r',c') | r' <- bl r, c' <- bl c ]

nrcSubGrid :: Sudoku -> (Row, Column) -> [Value]
nrcSubGrid s (r,c) = [s (r',c') | r' <- nrcbl r, c' <- nrcbl c ]
```

`freeInSubGrid`: From all the possible values, substract the one from the constraint, output all values. Here, we add the same case for the NRC subgrids.
``` haskell
freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid s (r,c) = freeInSeq (subGrid s (r,c))

freeInNRCgrid :: Sudoku -> (Row, Column) -> [Value]
freeInNRCgrid s (r,c) = freeInSeq (nrcSubGrid s (r,c))
```

`subgridInjective`: No duplicates in the mapping, add the same case for nrcSubGrids in the filter of `nrcSubGridInjective`
``` haskell
subgridInjective :: Sudoku -> (Row,Column) -> Bool
subgridInjective s (r,c) = injective vs 
                         where vs = filter (/= 0) (subGrid s (r,c))

nrcSubGridInjective :: Sudoku -> (Row, Column) -> Bool
nrcSubGridInjective s (r,c) = injective vs
                            where vs = filter (/= 0) (nrcSubGrid s (r,c))
```


Finally, add the `nrcbl` predicate to the `prune` function. Else, the added subgrids will not be pruned from the tree and the solver will run indefinately
``` haskell
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
```
