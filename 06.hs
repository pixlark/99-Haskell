isPalindrome :: Eq a => [a] -> Bool
isPalindrome []     = True
isPalindrome (x:[]) = True
isPalindrome lst = if f == l
                   then isPalindrome $
                        take ((length xs) - 1) xs
                   else False
  where f = head lst
        l = last lst
        xs = tail lst
