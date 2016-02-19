import Control.Applicative
import Control.Monad
import Data.List

main = do n <- readLn
          xs <- fmap (map read . words) getLine
          let ys = solve n xs
          mapM_ print ys

solve :: Int -> [Int] -> [Int]
solve n xs = f (sort xs) n
    where
        f _ 0 = []
        f (x:xs) n = let ys = takeWhile (<= x) xs
                         k = length ys
                         xs' = drop k xs
                         n' = (n - k - 1)
                      in n:f xs' n'
