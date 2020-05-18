quicksort :: (Ord a) => [a] -> [a]

quicksort [] = []
quicksort (x:xs) =
  quicksort (smaller) ++ x : quicksort (bigger)
  where 
    smaller = filter (<x) xs
    bigger  = filter (>=x) xs
