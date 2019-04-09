compress :: Eq a => [a] -> [a]
compress xs = helper [] xs
  where helper acc [] = reverse acc
        helper [] (x:xs) = helper [x] xs
        helper acc (x:xs) = if (head acc) == x
                              then helper acc xs
                              else helper (x:acc) xs
