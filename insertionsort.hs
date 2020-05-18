insort :: (Ord a) => [a] -> [a]
insort xs = 
  insort' xs [] 
  where
    insort' [] acc = acc
    insort' (x:xs) acc = insort' xs (insert' x acc)

insert' :: (Ord a) => a -> [a] -> [a] 
insert' x [] = [x]
insert' x z@(y : ys) 
  | x <= y = x : z
  | otherwise =  y : insert' x ys
