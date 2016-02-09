import Control.Monad
import qualified Data.Map as M
import Data.Map ((!))
import Data.Maybe
import qualified Data.Sequence as S
import Data.Sequence ((><))

type Graph = M.Map Int [Int]

main = do
  t <- readLn
  replicateM_ t $ do
    [n, m] <- fmap (map read . words) getLine
    edges <- replicateM m $ fmap (map read . words) getLine
    s <- readLn
    let dists = solve s n (asGraph n edges)
        distsExceptS = map fst $ filter ((/= s) . snd) $ zip dists [1..]
    putStrLn $ unwords $ map show distsExceptS


asGraph :: Int -> [[Int]] -> Graph
asGraph n xs = f xs (M.fromList (zip [1..n] (repeat [])))
  where
    f [] m = m
    f (x:xs) m = let from = x !! 0
                     to = x !! 1
                     m' = M.update (\a -> Just $ from:a) to m
                     m'' = M.update (\a -> Just $ to:a) from m'
                  in f xs m''


solve :: Int -> Int -> Graph -> [Int]
solve s n graph = f graph (S.singleton (s, 0)) (M.fromList (zip [1..n] (repeat (-1))))

  where
    f :: Graph -> S.Seq (Int, Int) -> M.Map Int Int -> [Int]
    f graph qs dists | S.null qs = M.elems dists
                     | otherwise = let (a, d) = S.index qs 0
                                       qs' = S.drop 1 qs
                                   in if (dists ! a) /= (-1)
                                        then f graph qs' dists
                                        else let dists' = M.insert a d dists
                                                 neighbours = graph ! a
                                                 qs'' = qs' >< (S.fromList $ zip neighbours (repeat (d + 6)))
                                              in f graph qs'' dists'

