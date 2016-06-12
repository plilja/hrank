main = do
    inp <- fmap (map read . tail . words) getContents
    let cache = solve 100
    mapM_ putStrLn $ map (\i -> if cache !! (i - 1) then "First" else "Second") inp

solve :: Int -> [Bool]
solve n = f n [True, True, True, True, False]
    where
        f 0 xs = reverse xs
        f n xs = f (n - 1) ((not (xs !! 1) || not (xs !! 2) || not (xs !! 4)):xs)
