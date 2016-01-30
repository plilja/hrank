import Data.List
import Control.Monad
import Control.Applicative

main = do [n, k] <- map read . words <$> getLine
          flowers <- map read . words <$> getLine
          print $ solve flowers k

solve :: [Integer] -> Integer -> Integer
solve xs k = go (reverse (sort xs)) k 0 0
  where
    go [] _ _ acc = acc
    go (x:xs) k i acc = let price = x * ((i `div` k) + 1)
                         in go xs k (i+1) (acc + price)
