import Control.Applicative
import Data.List

inf = 10^6

main = do (n:_) <- map read . words <$> getLine
          as <- map read . words <$> getLine
          let rs = solve n as
          putStrLn $ unwords $ map show rs

solve :: Int -> [Int] -> [Int]
solve n as = let (_, factors) = solve' n (sort as)
              in case factors of
                   [] -> [-1]
                   _ -> reverse $ foldl (\acc x -> x * (head acc) : acc) [1] factors

solve' :: Int -> [Int] -> (Int, [Int])
solve' 1 _ = (0, [])
solve' _ [] = (inf, [])
solve' n (a:as) | n `rem` a == 0 = let (size, fact) = solve' (n `div` a) (a:as)
                                   in min (size + 1, a:fact) (solve' n as)
                | otherwise  = solve' n as

