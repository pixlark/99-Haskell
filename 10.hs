encode :: Eq a => [a] -> [(Int, a)]
encode xs = helper [] xs
  where helper acc [] = reverse acc
        helper [] (x:xs) = helper [(1, x)] xs
        helper ((l, a):as) (x:xs) = if a == x
                                    then helper ((l + 1, a):as) xs
                                    else helper ((1, x):(l, a):as) xs
