import Control.Applicative
import Control.Monad
import Data.List

m = 10^9 + 7

main = do t <- readLn
          replicateM_ t $ do
             getLine -- ignore
             cols <- map read . words <$> getLine
             rows <- map read . words <$> getLine
             let r = solve cols rows
             print r

solve :: [Integer] -> [Integer] -> Integer
solve cols rows = let r = go (reverse (sort cols)) 1 (reverse (sort rows)) 1 0
                   in r `mod` m
  where
    go :: [Integer] -> Integer -> [Integer] -> Integer -> Integer -> Integer
    go [] i xs j acc = acc + (sum $ zipWith (*) xs (repeat i))
    go ys i [] j acc = acc + (sum $ zipWith (*) ys (repeat j))
    go (y:ys) i (x:xs) j acc
        | y > x = go ys (i+1) (x:xs) j (acc + y*j)
        | otherwise = go (y:ys) i xs (j+1) (acc + x*i)
