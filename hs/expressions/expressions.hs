import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.List as L

main = do getLine
          xs <- fmap (map read . words) getLine
          let ans = solve xs
          putStrLn $ unwords ans

solve :: [Int] -> [String]
solve (x:xs) = f xs (M.singleton x [show x])
    where
        f :: [Int] -> M.Map Int [String] -> [String]
        f [] m = reverse $ m ! 0
        f (x:xs) m = let newM = M.foldWithKey (combine x) M.empty m
                      in f xs newM

combine :: Int -> Int -> [String] -> M.Map Int [String] -> M.Map Int [String]
combine x v expr m = let m1 = M.insert ((v*x) `mod` 101) (show x:"*":expr) m
                         m2 = M.insert ((v+x) `mod` 101) (show x:"+":expr) m1
                         m3 = M.insert ((v-x) `mod` 101) (show x:"-":expr) m2
                      in m3
