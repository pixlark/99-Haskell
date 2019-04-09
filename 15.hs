repli :: [a] -> Int -> [a]
repli [] _ = []
repli (a:xs) i = (take i (repeat a)) ++ (repli xs i)
