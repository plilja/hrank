import Control.Monad

main = do t <- readLn
          replicateM_ t $ do
            xs <- getLine
            print $ solve xs

solve :: String -> Bool
solve xs = (equalCount xs 'R' 'G') &&
           (equalCount xs 'Y' 'B') &&
           (diffs xs 'R' 'G' 0) &&
           (diffs xs 'Y' 'B' 0)

equalCount xs c1 c2 = let n1 = length $ filter (==c1) xs
                          n2 = length $ filter (==c2) xs
                       in n1 == n2

diffs [] _ _ _ = True
diffs (x:xs) c1 c2 acc = let acc' = acc + if x == c1 then 1 else 0
                             acc'' = acc' + if x == c2 then -1 else 0
                          in (abs acc'') <= 1 && diffs xs c1 c2 acc''
