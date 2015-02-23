import Data.List

qspartition :: (Ord a) => [a] -> Int -> Int -> [a]
qspartition [] _ _ = []
qspartition xs i j | j == genericLength xs = (swapElts (i-1) 0 xs) -- need to change pivot place
                       | xs!!j < head xs = qspartition (swapElts i j xs) (i+1) (j+1)
                       | xs!!j > head xs = qspartition xs i (j+1)

-- https://gist.github.com/ijt/2010183
swapElts i j ls = [get k x | (k, x) <- zip [0..length ls - 1] ls]
    where get k x | k == i = ls !! j
                  | k == j = ls !! i
                  | otherwise = x

