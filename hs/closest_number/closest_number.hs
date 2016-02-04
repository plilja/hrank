import Data.List
import Data.Ord

main = do getLine
          xs <- fmap (map (read :: String -> Int) . words) getLine
          let r = solve xs
          putStrLn $ unwords $ map (\(a, b) -> show a ++ " " ++ show b) r

solve :: [Int] -> [(Int, Int)]
solve xs = let sorted = sort xs
               pairs = zip sorted (tail sorted)
               sortedByAbsDiff = sortBy (comparing pairAbsDiff) pairs
            in head $ groupBy (\a b -> pairAbsDiff a == pairAbsDiff b) sortedByAbsDiff

pairAbsDiff (a, b) = abs (a - b)
