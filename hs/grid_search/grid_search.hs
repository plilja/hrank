import Control.Applicative
import Control.Monad
import Data.Char
import Data.List

main = do
    t <- readLn
    replicateM_ t $ do
       [r, c] <- map (read :: String -> Int) . words <$> getLine
       xss <- replicateM r $ filter isDigit <$> getLine
       [r', c'] <- map (read :: String -> Int) . words <$> getLine
       yss <- replicateM r' $ filter isDigit <$> getLine
       if solve r xss r' yss
          then putStrLn "YES"
          else putStrLn "NO"

solve :: Int -> [String] -> Int -> [String] -> Bool
solve _ [] _ _ = False
solve r haystacks r' needles
    | r < r' = False
    | otherwise = let indices = map (\(h, n) -> findSubInd h n) (zip haystacks needles)
                      solution = not $ null $ foldl merge (head indices) (tail indices)
                   in solution || solve (r - 1) (tail haystacks) r' needles

merge [] _ = []
merge _ [] = []
merge (x:xs) (y:ys)
   | x < y = merge xs (y:ys)
   | x == y = x:(merge xs ys)
   | otherwise = merge (x:xs) ys

findSubInd xs ys = reverse $ foldl (\acc (t, i) -> if isPrefixOf ys t then i:acc else acc) [] (zip (tails xs) [0..])
