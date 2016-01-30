import Control.Monad
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe

main = do n <- readLn
          children <- replicateM n readLn
          print $ solve children

solve :: [Int] -> Int
solve cs = let sorted = L.sort (zip cs [0..])
               allocations = foldl f M.empty sorted
           in M.fold (\(alloc, count) acc -> acc+alloc) 0 allocations
  where
    f m (c, i) = let (n1, c1) = fromMaybe (0, 0) $ M.lookup (i - 1) m
                     (n2, c2) = fromMaybe (0, 0) $ M.lookup (i + 1) m
                     alloc1 = if c1 == c then 0 else n1
                     alloc2 = if c2 == c then 0 else n2
                     alloc = 1 + max alloc1 alloc2
                  in M.insert i (alloc, c) m
