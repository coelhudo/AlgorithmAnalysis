module Main (main,sort,swap,qspart) where
--module Main (qspartition) where

import qualified Data.List as DataList
import System.Environment (getArgs)

main = do
       args <- getArgs
       contents <- readFile $ args !! 0
       print . snd . Main.sort . map readInt . lines $ contents

readInt :: String -> Int
readInt = read

sort :: (Ord a) => [a] -> ([a],Int)
sort xs = quicksort xs

quicksort :: (Ord a) => [a] -> ([a],Int)
quicksort [] = ([],0)
quicksort [a] = ([a],1)
quicksort xs = (leftSorted ++ [pivot] ++ rightSorted, length leftSorted + length rightSorted + leftSortedTotal + rightSortedTotal)
            where (leftSorted,leftSortedTotal) = quicksort leftPartition
                  (rightSorted,rightSortedTotal) = quicksort rightPartition
                  leftPartition = take pivotIndex partitioned
                  rightPartition = drop (pivotIndex+1) partitioned
                  pivot = partitioned !! pivotIndex
                  (partitioned,pivotIndex) = qspartition xs

qspartition :: (Ord a) => [a] -> ([a], Int)
qspartition (x:xs) = qspart 0 0 x xs

qspart :: (Ord a) => Int -> Int -> a -> [a] -> ([a],Int)
qspart _ index pivot [] = ([pivot],index)
qspart j index pivot xs | j == DataList.genericLength xs = (pivot : xs,index)
                        | xs!!j < pivot = (xs!!j : list, listIndex)
                        | otherwise = qspart (j+1) index pivot xs
                        where (list,listIndex) = qspart j (index+1) pivot (drop 1 $ swap 0 j xs)

swap :: (Ord a) => Int -> Int -> [a] -> [a]
swap i j xs | i > j = swap j i xs
            | i < j = take i xs ++ [xs!!j] ++ (drop (i+1) $ take j xs) ++ [xs!!i] ++ drop (j+1) xs
            | otherwise = xs
