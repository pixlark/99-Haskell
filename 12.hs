data RLE a = Coded Int a
           | Indep a
  deriving(Show)

example = [Coded 4 'a', Indep 'b', Coded 2 'c', Coded 2 'a', Indep 'd', Coded 4 'e']

decode :: [RLE a] -> [a]
decode xs = decode' [] xs
  where decode' acc [] = reverse acc
        decode' acc ((Indep x):xs) = decode' (x:acc) xs
        decode' acc ((Coded l x):xs) = decode' ((take l (repeat x)) ++ acc) xs
