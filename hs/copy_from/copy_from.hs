import Control.Monad
import Control.Applicative

main = do t <- readLn
          replicateM_ t $ do
             n <- getLine
             xs <- map read . words <$> getLine
             let r1 = cont xs 0 (minBound :: Int)
             let r2 = nonCont xs
             putStrLn $ show r1 ++ " " ++ show r2

cont :: [Int] -> Int -> Int -> Int
cont [] _ best = best
cont (x:xs) acc best
    | acc + x < 0 = cont xs 0 (max best (acc + x))
    | otherwise = cont xs (acc + x) (max best (acc + x))

nonCont :: [Int] -> Int
nonCont xs = let pos = filter (>0) xs
              in if null pos
                    then maximum xs
                    else sum pos
