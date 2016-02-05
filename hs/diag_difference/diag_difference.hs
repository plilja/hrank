import Control.Applicative

main = do
    n <- readLn
    matrix <- readMatrix n
    print $ solve n matrix

readMatrix 0 = return []
readMatrix n = do row <- map read . words <$> getLine
                  rows <- readMatrix (n - 1)
                  return (row:rows)

solve n xs = let diag1 = map (\i -> xs !! i !! i) [0..n-1]
                 diag2 = map (\i -> xs !! i !! (n - i - 1)) [0..n-1]
             in abs $ sum $ zipWith (-) diag1 diag2
