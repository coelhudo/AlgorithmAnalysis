module Main (main,count,countFromFile) where

import Data.List

merge :: (Ord a,  Num c) => ([a], [a], c) -> ([a], c)
merge ([x], [y], ic) | x <= y = (x : y : [],  ic)
                     | otherwise = (y : x : [],  ic + 1)
merge (xs, [], ic) = (xs, ic)
merge ([], ys, ic) = (ys, ic)
merge ((x:xs), (y:ys), ic) | x <= y = (x : fst leftMerge, snd leftMerge)
                           | otherwise = (y : fst rightMerge, snd rightMerge)
                           where leftMerge = (merge (xs, (y:ys), ic))
                                 rightMerge = (merge ((x:xs), ys, ic))

f :: (Ord a, Num c) => ([a], c) -> ([a], c) -> ([a], [a], c)
f (xs, lic) (ys, ric) = (xs, ys, lic + ric + (genericLength [(x,y) | x <- xs, y <- ys, x > y]))

mergesort :: (Ord a, Num c) => ([a], c) -> ([a], c)
mergesort (xs, ic) = merge (if len > 2 then (f (mergesort (left, ic)) (mergesort (right, ic))) else (left, right, ic))
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

