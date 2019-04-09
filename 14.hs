dupli :: [a] -> [a]
dupli [] = []
dupli (a:xs) = a:a:(dupli xs)
