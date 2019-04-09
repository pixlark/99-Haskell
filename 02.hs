stl :: [a] -> Maybe a
stl (a:b:[]) = Just a
stl (a:b:xs) = stl xs
stl _ = Nothing
