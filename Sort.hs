module Sort (quicksort) where

quicksort :: (Ord b) => (a -> b) -> [a] -> [a]
quicksort _ [] = []
quicksort f (x:xs) = left ++ (x : right)
    where left = quicksort f $ filter (\y -> f y < f x) xs
          right = quicksort f $ filter (\y -> f y >= f x) xs
