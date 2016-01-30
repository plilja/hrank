import Control.Applicative
import Control.Monad
import qualified Data.Map as M

main = do [n, k] <- map read . words <$> getLine
          xs <- map read . words <$> getLine
          putStrLn $ unwords $ map show $ solve xs (min n k)

solve :: [Int] -> Int -> [Int]
solve xs k = let valToIdx = M.fromList $ zip xs [0..]
                 idxToVal = M.fromList $ zip [0..] xs
              in reverse $ solve' k valToIdx idxToVal []

solve' :: Int -> M.Map Int Int -> M.Map Int Int -> [Int] -> [Int]
solve' k valToIdx idxToVal acc
     | k == 0 = (reverse (M.elems idxToVal))++ acc
     | M.null valToIdx = acc
     | otherwise = let ((maxVal, i), valToIdx') = M.deleteFindMax valToIdx
                       ((j, first), idxToVal') = M.deleteFindMin idxToVal
                    in if i == j
                      then solve' k valToIdx' idxToVal' (maxVal:acc)
                      else solve' (k-1) (M.insert first i valToIdx') (M.insert i first idxToVal') (maxVal:acc)

