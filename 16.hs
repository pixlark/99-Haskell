dropEvery :: [a] -> Int -> [a]
dropEvery xs r = dropEvery' xs r 1
  where dropEvery' [] _ _ = []
        dropEvery' (x:xs) r i = if (i `mod` r) == 0
                                then dropEvery' xs r (i + 1)
                                else x:(dropEvery' xs r (i + 1))
