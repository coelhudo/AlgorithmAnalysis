module Main (main,sort) where

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
quicksort [a] = ([a],0)
quicksort xs = (leftSorted ++ [pivot] ++ rightSorted, length xs - 1 + leftSortedTotal + rightSortedTotal)
            where (leftSorted,leftSortedTotal) = quicksort leftPartition
                  (rightSorted,rightSortedTotal) = quicksort rightPartition
                  leftPartition = take pivotIndex partitioned
                  rightPartition = drop (pivotIndex+1) partitioned
                  pivot = partitioned !! pivotIndex
                  (partitioned,pivotIndex) = qspartition xs

qspartition :: (Ord a) => [a] -> ([a], Int)
qspartition xs = qspartitionMedian xs

qspartitionFirst :: (Ord a) => [a] -> ([a], Int)
qspartitionFirst (x:xs) = (swap 0 index (x:partitioned),index)
                     where (partitioned,index) = qspart 0 0 x xs

qspartitionLast :: (Ord a) => [a] -> ([a], Int)
qspartitionLast xs = (swap 0 index (last xs:partitioned),index)
                     where (partitioned,index) = qspart 0 0 (last xs) ((init $ drop 1 xs) ++ [head xs])

qspartitionMedian :: (Ord a) => [a] -> ([a], Int)
qspartitionMedian [x,y] | x > y = ([y,x],0)
                        | otherwise = ([x,y],0)
qspartitionMedian xs = (swap 0 index (pivot:partitioned),index)
                       where (partitioned,index) = qspart 0 0 pivot (drop 1 $ swap 0 pivotIndex xs)
                             (pivot,pivotIndex) = selectMedian firstElement middleElement lastElement
                             firstElement = (head xs,0)
                             lastElement = (last xs, length xs - 1)
                             middleElement = getMiddleElement xs

getMiddleElement :: (Ord a) => [a] -> (a,Int)
getMiddleElement xs = (xs !! index, index)
                      where size = length xs
                            index = if odd size then size `div` 2 else size `div` 2 - 1

selectMedian :: (Ord a) => (a,Int) -> (a,Int) -> (a,Int) -> (a,Int)
selectMedian first middle last = let xs = DataList.sort [first,middle,last] in xs !! 1

qspart :: (Ord a) => Int -> Int -> a -> [a] -> ([a],Int)
qspart _ index pivot [] = ([],index)
qspart j index pivot xs | j == DataList.genericLength xs = (xs,index)
                        | xs!!j < pivot = (xs!!j : list, listIndex)
                        | otherwise = qspart (j+1) index pivot xs
                        where (list,listIndex) = qspart j (index+1) pivot (drop 1 $ swap 0 j xs)

swap :: (Ord a) => Int -> Int -> [a] -> [a]
swap i j xs | i > j = swap j i xs
            | i < j = take i xs ++ [xs!!j] ++ (drop (i+1) $ take j xs) ++ [xs!!i] ++ drop (j+1) xs
            | otherwise = xs
