elementAt :: Integer -> [a] -> a
elementAt 0 (x:_)  = x
elementAt i (x:xs) = elementAt (i - 1) xs
