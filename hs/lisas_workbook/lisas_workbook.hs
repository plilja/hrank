import Control.Monad

main = do
    [n, k] <- readInts
    chapters <- readInts
    print $ solve n k chapters

readInts :: IO [Int]
readInts = liftM (map read . words) getLine

solve n k chapters = go n k chapters 1 1
    where
        go _ _ [] _ _ = 0
        go n k (c:chapters) page x | x > c = go n k chapters page 1
                                   | x <= page && page <= (min c (x + k - 1)) = 1 + go n k (c:chapters) (page+1) (x+k)
                                   | otherwise = go n k (c:chapters) (page+1) (x+k)
