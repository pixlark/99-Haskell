import Data.List

lsort :: [[a]] -> [[a]]
lsort = sortBy (\a b -> compare (length a) (length b))

lfsort :: [[a]] -> [[a]]
lfsort lst = sortBy comparator lst
  where lengths = map length lst
        total = length lengths
        freq x = (fromIntegral $ length $ elemIndices x lengths :: Float) / (fromIntegral $ total :: Float)
        comparator a b = compare (freq $ length a) (freq $ length b)
