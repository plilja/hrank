import Control.Monad
import Control.Applicative

main = do q <- readLn
          xss <- replicateM q $ map (read :: String -> Int) . words <$> getLine
          let f xs ys
               | null xs || null ys = []
               | head xs < head ys = f (drop 2 xs) ys
               | head xs == head ys = [xs !! 0, min (xs !! 1) (ys !! 1)] ++ f (drop 2 xs) (drop 2 ys)
               | otherwise = f xs (drop 2 ys)
          let r = foldl f (head xss) (tail xss)
          putStrLn $ unwords $ map show r
