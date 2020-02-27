-- from 31
isPrime n = not $ or $ map (\x -> (n `mod` x) == 0) [2..top]
  where top = floor $ sqrt $ fromIntegral n
--

lowestPrimeFactor :: Int -> Int
lowestPrimeFactor x
  | x < 2 = error "Integer must be >= 2"
  | otherwise = tryFactors 2
  where tryFactors f = 
          if isPrime f
          then if (x `mod` f) == 0
               then f
               else tryFactors (f + 1)
          else tryFactors (f + 1)

primeFactors :: Int -> [Int]
primeFactors = reverse . helper []
  where helper acc n = if (isPrime r) && (isPrime f)
                       then r:f:acc
                       else helper (f:acc) r
          where f = lowestPrimeFactor n
                r = n `div` f
