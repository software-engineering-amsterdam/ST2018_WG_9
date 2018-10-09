# Exercise 5 - Time: 10m 
Due to exercise 1, all the code needed to generate NRC sudokus is already present. Thus, we can call 

``` haskell
main :: IO ()
main = do [r] <- rsolveNs [emptyN]
          showNode r
          s  <- genProblem r
          showNode s
```
To generate sudoku problems with the NRC constraint. 

## Bonus: NRC problems with empty blocks - Time: 20m for writing it down
We now have the possibility to generate NRC Sudoku problems with the added contraints. If we look back at Exercise 4, we can also define NRC problems with empty blocks using Exercise 4's code implementation  

The generator generates an nrc sudoku *with* the added constraint `blockNrcConstrnt` that has been  defined in Lecture5.hs.  


## Example of a valid sudoku problem that has the NRC constraint with 3 empty blocks
```
*Lab5> empty3BlockGen
+-------+-------+-------+
| 4 3 5 |       | 7   2 |
|  +----|--+ +--|----+  |
|  |    |  | |  | 1  |8 |
|  |    |  |5|2 |    |  |
+-------+-------+-------+
|  |    |  | |  |    |  |
|  +----|--+ +--|----+  |
|   1 8 |       |       |
|  +----|--+ +--|----+  |
|  |    |  | |  |    |  |
+-------+-------+-------+
|  |2   | 8| |  |    |  |
|  |6   | 4| |  |    |  |
|  +----|--+ +--|----+  |
|   4   |   3 9 |       |
+-------+-------+-------+

```
## Example of a valid sudoku problem that has the NRC constraint with 4 empty blocks
```
*Lab5> empty4BlockGen
+-------+-------+-------+
| 9     |       |   7   |
|  +----|--+ +--|----+  |
| 8|  3 |  | |  |   5|  |
|  |    |  | |  |   1|9 |
+-------+-------+-------+
|  |  1 | 5| |  |    |  |
|  +----|--+ +--|----+  |
|   4 2 |       |       |
|  +----|--+ +--|----+  |
| 5|  7 |  |4|  |    |  |
+-------+-------+-------+
|  |    |  | |6 |    |  |
|  |    | 4| |  |    |  |
|  +----|--+ +--|----+  |
|       |       |       |
+-------+-------+-------+
```
## Example of a valid sudoku problem that has the NRC constraint with 5 empty blocks
```
*Lab5> empty5BlockGen
+-------+-------+-------+
| 5     |       | 6   9 |
|  +----|--+ +--|----+  |
|  |8   |  | |  |    |  |
|  |  4 |  | |  | 2 3|7 |
+-------+-------+-------+
|  |    |  | |  |    |  |
|  +----|--+ +--|----+  |
|       |       |       |
|  +----|--+ +--|----+  |
|  |    |  | |  |    |  |
+-------+-------+-------+
|  |  7 |  | |  |   9|  |
|  |2 3 |  | |  | 8  |  |
|  +----|--+ +--|----+  |
|   6   |       | 3 5 1 |
+-------+-------+-------+
```

Apparently the extra constraint includes the possibility to define a problem with 5 empty blocks
