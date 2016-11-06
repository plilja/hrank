import Data.List
import qualified Data.Map as M
import Data.Maybe

main = do x <- readLn
          n <- readLn
          print $ solve x n

solve :: Int -> Int -> Int
solve x n = let powers = takeWhile (<=x) (map (^n) [1..])
                m = solve' powers (M.singleton 0 1)
             in fromMaybe 0 $ M.lookup x m

solve' :: [Int] -> M.Map Int Int -> M.Map Int Int
solve' [] m = m
solve' (p:ps) m = let m' = M.foldWithKey (\k v acc -> M.insertWith (+) (k+p) v acc) m m
                   in solve' ps m'
