import Data.List
import Control.Monad

main = do
  n <- readLn
  k <- readLn
  xs <- replicateM n readLn
  print $ solve k xs

solve :: Int -> [Integer] -> Integer
solve k xs = let sorted = sort xs
              in minimum $ zipWith (-) (drop (k - 1) sorted) sorted
