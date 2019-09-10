removeAt :: Int -> [a] -> [a]
removeAt _ [] = error "Beyond list bounds"
removeAt 0 (x:xs) = xs
removeAt i (x:xs) = x:(removeAt (i - 1) xs)

removeAtTail :: Int -> [a] -> [a]
removeAtTail i xs = removeAt' [] i xs
  where removeAt' _ _ [] = error "Beyond list bounds"
        removeAt' acc 0 (x:xs) = (reverse acc) ++ xs
        removeAt' acc i (x:xs) =
          removeAt' (x:acc) (i - 1) xs
