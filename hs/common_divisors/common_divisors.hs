import Control.Monad

main = do t <- readLn
          replicateM_ t $ do
             [m, l] <- fmap (map read . words) getLine
             print $ solve m l

solve m l = let g = gcd m l
             in length $ factors g

factors x = x:[i | i <- [1..(x `div` 2)], x `mod` i == 0]
