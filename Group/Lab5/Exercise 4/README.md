# Exercise 4 - Time: 1h

The generator generates a sudoku *without* the added constraint `blockNrcConstrnt` that has been  defined in Lecture5.hs. 

The implementation with explanation can be found in `Lab5.hs`. Note that, if no valid problem can be found with the initial random values chosen by the generator, the problem is returned with a lingering node from the tree. However, the blocks from this lingering node are removed.  

To clarify, consider the example below. This is an invalid sudoku problem that was returned by `*Lab5> empty5BlockGen`
```
*Lab5> empty5BlockGen
+-------+-------+-------+
|       |       | 1 9 3 |
|       |       | 7 4 8 |
|       |       | 5 2 6 |
+-------+-------+-------+
|       | 2 7 6 | 3 8 5 |
|       | 9 4 8 | 2 7 1 |
|       | 1 5 3 | 4 6 9 |
+-------+-------+-------+
|       |       | 8 1 4 |
|       |       | 9 5 7 |
|       |       | 6 3 2 |
+-------+-------+-------+
```

## Example of a valid sudoku problem with 3 empty blocks
```
*Lab5> empty3BlockGen
+-------+-------+-------+
| 5   2 |     8 | 1     |
|     1 |       | 8   2 |
| 6     |   9   | 7     |
+-------+-------+-------+
|       |       |       |
| 8     |       | 5     |
|   3   |       |       |
+-------+-------+-------+
|       |   5 6 |       |
|       | 9   4 |       |
|       | 8     |       |
+-------+-------+-------+
```
## Example of a valid sudoku problem with 4 empty blocks
```
*Lab5> empty4BlockGen
+-------+-------+-------+
|       | 9     |   5   |
|       | 1     |     6 |
|       |     5 |       |
+-------+-------+-------+
|   4   | 3   1 |       |
|     5 | 2     |       |
| 9   8 |   5 4 |       |
+-------+-------+-------+
|       |       | 6 3 7 |
|       |       |       |
|       |       |   9 2 |
+-------+-------+-------+
```
## Example of a valid sudoku problem with 5 empty blocks
There is no example for this case.  
Additionally, this has been proven:  
https://puzzling.stackexchange.com/questions/309/what-is-the-maximum-number-of-empty-3x3-blocks-a-proper-sudoku-can-have
