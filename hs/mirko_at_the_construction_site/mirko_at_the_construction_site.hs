import qualified Data.Map as M
import qualified Data.List as L
import Control.Monad
import Data.Ord
import Data.Maybe

data Line = Line {yIntercept :: Integer, slope :: Integer, index :: Integer}

main = do [n, q] <- fmap (map read . words) getLine
          bs <- fmap (map read . words) getLine
          ss <- fmap (map read . words) getLine
          qs <- replicateM q readLn
          let as = solve bs ss qs
          mapM_ print as

solve :: [Integer] -> [Integer] -> [Integer] -> [Integer]
solve bs ss qs = let highest = determineHighest bs ss
                  in map (\q -> snd $ fromJust $ M.lookupLE q highest) qs

determineHighest :: [Integer] -> [Integer] -> M.Map Integer Integer
determineHighest bs ss = let lines = reverse $ map toLine (zip (zip bs ss) [1..])
                             sortedWithIdx = L.sortBy (comparing (negate . yIntercept)) lines
                          in M.fromList $ go sortedWithIdx []
    where
        go :: [Line] -> [(Line, Integer)] -> [(Integer, Integer)]
        go [] acc = map (\(line, x) -> (x, index line)) acc
        go (line:xs) [] = go xs [(line, 0)]
        go (l1:xs) ((l2, x):as) | eval l1 x > eval l2 x = go (l1:xs) as
                                | (slope l1) > (slope l2) = let ins = intersection l1 l2
                                                            in if index l1 < index l2 && eval l1 ins == eval l2 ins
                                                                   then go xs ((l1, ins + 1):(l2, x):as)
                                                                   else go xs ((l1, ins):(l2, x):as)
                                | otherwise = go xs ((l2, x):as)

eval (Line y m i) x = y + m*x

intersection :: Line -> Line -> Integer
intersection (Line y1 m1 i1) (Line y2 m2 i2) = ceiling $ (fromIntegral (y1 - y2)) / (fromIntegral (m2 - m1))

toLine ((b, m), i) = Line b m i


