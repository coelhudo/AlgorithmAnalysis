merge :: (Ord a) => ([a],[a]) -> [a]
merge ([x],[y]) | x <= y = x : y : []
              | otherwise = y : x : []
merge (xs,[]) = xs
merge ([],ys) = ys
merge ((x:xs),(y:ys)) | x <= y = x : (merge (xs, (y:ys)))
                      | otherwise = y : (merge ((x:xs),ys))

mergesort :: (Ord a) => [a] -> [a]
mergesort xs = merge (if len > 2 then ((mergesort $ fst splitted), (mergesort $ snd splitted)) else splitted)
               where len = length xs
                     splitted = splitAt (len `div`2) xs
