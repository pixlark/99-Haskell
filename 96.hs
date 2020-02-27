import Data.Char (isAlpha)

identifier :: String -> Bool
identifier (c:[])
  | isAlpha c = True
  | otherwise = False
identifier (c:n:s)
  | isAlpha c = identifier (n:s)
  | c == '-' = if n == '-'
               then False
               else identifier (n:s)
