import Control.Applicative
import Data.List

solve :: [Int] -> [Int] -> [Int]
solve as bs = nub $ solve' (sort as) (sort bs)
   where
     solve' [] bs = bs
     solve' _ [] = []
     solve' (a:as) (b:bs) | a < b = solve' as (b:bs)
                          | a == b = solve' as bs
                          | a > b = b:solve' (a:as) bs

main = do getLine
          as <- map read . words <$> getLine
          getLine
          bs <- map read . words <$> getLine
          let r = solve as bs
          putStrLn $ unwords $ map show r
