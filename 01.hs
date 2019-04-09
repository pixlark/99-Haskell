last_ :: [a] -> a
last_ (x:[]) = x
last_ (x:xs) = last_ xs

last_safe :: [a] -> Maybe a
last_safe []     = Nothing
last_safe (x:[]) = Just x
last_safe (x:xs) = last_safe xs
