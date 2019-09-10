slice :: [a] -> Integer -> Integer -> [a]
slice l s e = take' (drop' l s)
                    (e - s + 1)
  where take' l 0 = []
        take' [] n = error "List not large enough"
        take' (x:xs) n = x:(take' xs (n - 1))
        drop' l 0 = l
        drop' [] n = error "List is not large enough"
        drop' (x:xs) n = drop' xs (n - 1)
