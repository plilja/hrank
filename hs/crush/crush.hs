import Control.Applicative
import Control.Monad
import Data.List

main = do [n, m] <- map read . words <$> getLine
          ops <- replicateM m $ map read . words <$> getLine
          print $ solve n ops

solve :: Int -> [[Integer]] -> Integer
solve n ops = let events = sort $ concat $ map (\[a, b, k] -> [(a, k), (b+1, -k)]) ops
               in maximum $ f events [(0, 0)]
  where
     f [] acc = map fst acc
     f ((i, k1):xs) ((k2, j):acc) | i == j = f xs ((k1+k2, i):acc)
                                  | otherwise = f xs ((k1+k2, i):(k2,j):acc)
