import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import qualified Data.List as L
import Text.Printf
import Data.Maybe

main = do t <- readLn
          replicateM_ t $ do
             m <- readLn :: IO Int
             n <- readLn :: IO Int
             prices <- map (read :: String -> Int) . words <$> getLine
             let (i1, i2) = solve m prices
             printf "%d %d\n" i1 i2

solve :: Int -> [Int] -> (Int, Int)
solve m prices = let priceToIndex = priceToIndexMap prices
                     (i1:i2:_) = concat $ map (\(p, i) -> L.delete i (M.findWithDefault [] (m - p) priceToIndex)) (zip prices [1..])
                  in (min i1 i2, max i1 i2)

priceToIndexMap :: [Int] -> M.Map Int [Int]
priceToIndexMap prices = foldl (\acc (p, i) -> M.insertWithKey (\k a b -> a++b) p [i] acc) M.empty (zip prices [1..])


