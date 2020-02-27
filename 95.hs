import Data.List (intercalate)
import Data.Char (ord)

fullWords :: Int -> String
fullWords n
  | n < 0 = error "No negative numbers"
  | otherwise = intercalate "-" $ map digitToWord (show n)
  where digitToWord c = ["zero", "one",
                         "two", "three",
                         "four", "five",
                         "six", "seven",
                         "eight", "nine"]
                        !! ((ord c) - (ord '0'))
