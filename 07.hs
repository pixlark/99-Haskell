data Nested a = Elem a
              | List [Nested a]

flatten :: (Nested a) -> [a]
flatten (Elem e) = [e]
flatten (List n) = foldl (++) [] (map flatten n)
