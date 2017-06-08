> import Data.List

This code solves Sudoku puzzles using a magical function called Loeb.

Loeb allows you to define a list of functions which resolves into a list
of values, allowing the list to self-reference. In the case of Sudoku, the
solver code will terminate only in the case where the Sudoku puzzle has
a unique solution.

The code starts with a 1-dimensional list (representing the 2-dimensional
Sudoku board), where `0` represents the "holes" that need to be computed.

This list is converted into a list of functions, with any already-known
value `x` replaced with `const x`, and with the holes converted into
functions that calculate the value by indexing into the list of functions
itself.

This list of functions is then given to `loeb`, which resolves them
recursively.

> loeb :: Functor f => f (f a -> a) -> f a
> loeb x = go where go = fmap ($ go) x

Takes in an index and returns a function from a list to a value.
The function it returns calculates the correct value for the hole
based on what value is missing from that hole's row and column.

> holeAt :: (Num a, Eq a, Ord a) => Int -> [a] -> a
> holeAt index = findVal indices
>     where row         = index `div` puzzleSize
>           col         = index `mod` puzzleSize
>           rowIndices  = [puzzleSize * row + i | i <- [0..puzzleSize - 1]]
>           colIndices  = [puzzleSize * i + col | i <- [0..puzzleSize - 1]]
>           indices     = flip (\\) [index] . nub $ rowIndices ++ colIndices
>           missingVals = (\\) [1, 2, 3, 4]
>           valsAt      = flip $ map . (!!)
>           findVal i   = head . missingVals . nub . (valsAt i)

Convert a list of values into a list of functions
calculating values. The representation resulting
from this is what `loeb` will be applied to.

> process :: (Num a, Eq a, Ord a) => [a] -> [[a] -> a]
> process lst = zipWith valToFunc lst [0..]
>     where valToFunc val idx =
>               if val == 0
>               then holeAt idx
>               else const val

The size of the puzzle along both axes (assumes a square puzzle).

> puzzleSize = 4

The puzzle, where `0` indicates a value that needs to be calculated.

> problem = [ 3, 4, 2, 0
>           , 0, 1, 3, 4
>           , 1, 0, 4, 3
>           , 4, 3, 0, 2 ]

The expected solution for this puzzle.

> solution = [ 3, 4, 2, 1
>            , 2, 1, 3, 4
>            , 1, 2, 4, 3
>            , 4, 3, 1, 2 ]
>
> main = do
>     let attempt = loeb . process $ problem
>     if solution == attempt
>     then putStrLn $ "Success:  " ++ show attempt
>                ++ "\nExpected: " ++ show solution
>     else putStrLn $ "Failed:   " ++ show attempt
>                ++ "\nExpected: " ++ show solution

