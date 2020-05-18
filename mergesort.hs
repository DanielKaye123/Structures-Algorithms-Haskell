-- Recusively split the list in half
-- Merge sublist in ascending order

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [a] = [a]
mergesort xs = 
  merge (mergesort l) (mergesort r) 
  where
    (l, r) = splitAt ((length xs) `div` 2) xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (y:ys) (z:zs) 
  | y <= z = y : merge ys (z:zs)  
  | otherwise = z : merge (y:ys) (zs)

  
