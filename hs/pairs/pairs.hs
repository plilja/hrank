import qualified Data.Set as S

main = do
    [n, k] <- fmap (map read . words) getLine
    xs <- fmap (map read . words) getLine
    print $ solve k xs

solve :: Int -> [Int] -> Int
solve k xs = let ss = S.fromList xs
              in length $ filter (\x -> S.member (x - k) ss) xs
