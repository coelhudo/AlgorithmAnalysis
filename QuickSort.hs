import Data.List

qspart :: (Ord a) => [a] -> Int -> Int -> ([a],Int)
qspart [] _ _ = ([],0)
qspart xs i j | j == genericLength xs = ((swapElts (i-1) 0 xs),(i-1)) -- need to change pivot place
              | xs!!j < head xs = qspart (swapElts i j xs) (i+1) (j+1)
              | xs!!j > head xs = qspart xs i (j+1)

qspartition :: (Ord a) => [a] -> ([a],Int)
qspartition xs = qspart xs 1 1

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort [a] = [a]
quicksort xs = quicksort leftSide ++ [pivot] ++ quicksort rightSide
            where leftSide = take pivotPosition partitioned
                  rightSide = drop (pivotPosition+1) partitioned
                  pivot = partitioned !! pivotPosition
                  (partitioned,pivotPosition) = qspartition xs

-- https://gist.github.com/ijt/2010183
swapElts i j ls = [get k x | (k, x) <- zip [0..length ls - 1] ls]
    where get k x | k == i = ls !! j
                  | k == j = ls !! i
                  | otherwise = x

