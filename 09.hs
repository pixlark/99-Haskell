pack :: Eq a => [a] -> [[a]]
pack xs = helper [] xs
  where helper acc [] = reverse acc
        helper [] (x:xs) = helper [[x]] xs
        helper (a:as) (x:xs) = if (head a) == x
                               then helper ((x:a):as) xs
                               else helper ([x]:a:as) xs
