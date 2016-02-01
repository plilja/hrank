import Data.List

main = do
  getLine
  xs <- getInts
  [p, q] <- getInts
  print $ solve p q xs

getInts :: IO [Integer]
getInts = fmap (map read . words) getLine

solve :: Integer -> Integer -> [Integer] -> Integer
solve p q xs = let smallest = minimum xs
                   largest = maximum xs
                   start = min smallest (smallest - 2 * (smallest - p))
                   end = max largest (largest + 2 * (q - largest))
                   sorted = nub $ sort $ [start] ++ xs ++ [end]
                   pairs = zip sorted (tail sorted)
                   validPairs = filter (intervalIn (p, q)) pairs
                in snd $ minimum $ map (bestInInterval p q) validPairs

bestInInterval :: Integer -> Integer -> (Integer, Integer) -> (Integer, Integer)
bestInInterval p q (a, b) = let tmp = a + ((b - a) `div` 2)
                                pos = min (max p tmp) q
                                d = negate $ minimum $ map abs [a - pos, b - pos]
                         in (d, pos)

intervalIn (a, b) (c, d) = (pointIn a (c, d)) || (pointIn b (c, d)) || (a < c && b > d)
pointIn p (c, d) = p >= c && p <=d
