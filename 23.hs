import System.Random
import Data.List

remove :: Int -> [a] -> IO [a]
remove i l = return ((take i l) ++ (drop (i + 1) l))

rndSelect :: Eq a => [a] -> Int -> IO [a]
rndSelect l n = do removed <- toRemove l ((length l) - n)
                   return $ filter (\i -> elem i removed) l
  where toRemove l 0 = return l
        toRemove l n = do
          i <- getStdRandom $ randomR (0, (length l) - 1)
          nl <- remove i l
          toRemove nl (n - 1)
