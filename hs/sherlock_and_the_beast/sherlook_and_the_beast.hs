import Control.Monad

main = do
    t <- readLn
    replicateM_ t $ do
        n <- readLn
        putStrLn $ solve n

solve :: Int -> String
solve n = let fives = filter (\x -> x `mod` 3 == 0) [n,n-1..0]
              threes = filter (\x -> x `mod` 5 == 0) $ map (n -) fives
           in case threes of
               (t:_) -> replicate (n - t) '5' ++ replicate t '3'
               otherwise -> "-1"
