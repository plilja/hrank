import Data.List
import Control.Monad

main = do
    t <- readLn
    replicateM_ t $ do
        [n, k] <- fmap (map read . words) getLine
        xs <- fmap (map read . words) getLine
        let r = length $ filter (<= 0) xs
        if r < k
            then putStrLn "YES"
            else putStrLn "NO"
