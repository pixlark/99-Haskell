data RLE a = Coded Int a
           | Indep a
  deriving(Show)

encode :: Eq a => [a] -> [RLE a]
encode xs = encode' [] xs
  where encode' acc [] = reverse acc
        encode' [] (x:xs) = encode' [Indep x] xs
        encode' ((Indep a):as) (x:xs) = if x == a
                                       then encode' ((Coded 2 a):as) xs
                                       else encode' ((Indep x):(Indep a):as) xs
        encode' ((Coded l a):as) (x:xs) = if x == a
                                         then encode' ((Coded (l + 1) a):as) xs
                                         else encode' ((Indep x):(Coded l a):as) xs

