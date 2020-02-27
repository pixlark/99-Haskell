--isPrime :: Int -> Bool
isPrime n = not $ or $ map (\x -> (n `mod` x) == 0) [2..top]
  where top = floor $ sqrt $ fromIntegral n
