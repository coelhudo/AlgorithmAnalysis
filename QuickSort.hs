module Main (main,sort,swap,qspart) where

import qualified Data.List as DataList

main = do
       contents <- readFile "QuickSort.txt"
       print . snd . Main.sort . map readInt . lines $ contents

readInt :: String -> Int
readInt = read

sort :: (Ord a) => [a] -> ([a],Int)
sort xs = quicksort xs

quicksort :: (Ord a) => [a] -> ([a],Int)
quicksort [] = ([],0)
quicksort [a] = ([a],1)
quicksort xs = (leftSorted ++ [pivot] ++ rightSorted, leftSortedTotal + rightSortedTotal)
            where (leftSorted,leftSortedTotal) = quicksort leftPartition
                  (rightSorted,rightSortedTotal) = quicksort rightPartition
                  leftPartition = take pivotIndex partitioned
                  rightPartition = drop (pivotIndex+1) partitioned
                  pivot = partitioned !! pivotIndex
                  (partitioned,pivotIndex) = qspartition xs

qspartition :: (Ord a) => [a] -> ([a], Int)
qspartition xs = qspart 1 1 xs

qspart :: (Ord a) => Int -> Int -> [a] -> ([a], Int)
qspart _ _ [] = ([], 0)
qspart i j xs | j == DataList.genericLength xs = ((swap 0 (i - 1) xs),(i-1)) -- need to change pivot place
                | xs!!j < head xs = qspart (i+1) (j+1) (swap i j xs)
                | xs!!j > head xs = qspart i (j+1) xs

swap :: (Ord a) => Int -> Int -> [a] -> [a]
swap i j xs | i /= j = take i xs ++ [xs!!j] ++ (drop (i+1) $ take j xs) ++ [xs!!i] ++ drop (j+1) xs
                | otherwise = xs
