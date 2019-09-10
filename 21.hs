insertAt :: a -> [a] -> Int -> [a]
insertAt n xs 0 = n:xs
insertAt n (x:xs) i = x:(insertAt n xs (i - 1))
