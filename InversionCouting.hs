module Main (main,count) where

import Data.List

merge :: (Ord a,  Num c) => ([a], [a], c) -> ([a], c)
merge ([x], [y], ic) | x <= y = (x : y : [],  ic)
                     | otherwise = (y : x : [],  ic + 1)
merge (xs, [], ic) = (xs, ic)
merge ([], ys, ic) = (ys, ic)
merge ((x:xs), (y:ys), ic) | x <= y = (x : fst leftMerge, snd leftMerge)
                           | otherwise = (y : fst rightMerge, (genericLength xs) + (snd rightMerge))
                           where leftMerge = (merge (xs, (y:ys), ic))
                                 rightMerge = (merge ((x:xs), ys, ic + 1))

combineResults :: (Ord a, Num c) => ([a], c) -> ([a], c) -> ([a], [a], c)
combineResults (xs, lic) (ys, ric) = (xs, ys, lic + ric)

mergesort :: (Ord a, Num c) => ([a], c) -> ([a], c)
mergesort (xs, ic) = merge (if len > 2 then (combineResults (mergesort (left, ic)) (mergesort (right, ic))) else (left, right, ic))
               where len = length xs
                     splitted = splitAt (len `div`2) xs
                     left = fst splitted
                     right = snd splitted

count :: (Ord a, Num c) => [a] -> c
count xs = snd $ mergesort (xs, 0)

main = do
       contents <- readFile "IntegerArray.txt"
       print . count . map readInt . lines $ contents

readInt :: String -> Int
readInt = read

countFromFile = main

