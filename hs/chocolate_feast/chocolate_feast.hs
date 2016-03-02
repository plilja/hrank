import Control.Monad

main = do
    t <- readLn
    replicateM t $ do
        [n, c, m] <- fmap (map (read :: String -> Integer) . words) getLine
        print $ solve n c m 0

solve n c m w | w >= m = let r = w `div` m
                         in r + solve n c m (w + r - r*m)
              | n < c = 0
              | otherwise = let r = n `div` c
                             in r + solve (n - r*c) c m (w+r)
