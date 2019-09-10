rotate :: [a] -> Int -> [a]
rotate xs 0 = xs
rotate xs n
  | n < 0 = (drop (len + n) xs) ++
            (take (len + n) xs)  -- rotate right
  | n > 0 = (drop n xs) ++ (take n xs) -- rotate left
  where len = (length xs)
