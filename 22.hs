range :: Int -> Int -> [Int]
range s e
  | s >= e = []
  | s < e  = s:(range (s + 1) e)
