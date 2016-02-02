import Control.Monad

main = do
  t <- readLn
  replicateM t $ do
    n <- getLine
    prices <- fmap (map read . words) getLine
    print $ solve prices

solve :: [Integer] -> Integer
solve ps = let bestPrice = foldr (\p acc -> (max p (head acc)):acc) [0] ps
               dailyPriceToBest = zip ps bestPrice
            in sum $ map dailyWinnings dailyPriceToBest

dailyWinnings (p, b)
  | p < b = b - p
  | otherwise = 0
