import Control.Applicative
import Control.Monad
import Data.List
import Data.Ord

main = do t <- readLn :: IO Int
          replicateM_ t $ do
            (n:k:_) <- map read . words <$> getLine
            xs <- map read . words <$> getLine
            let rs = unwords $ map show $ solve k xs
            case rs of
              [] -> putStrLn "-1"
              _ -> putStrLn rs

solve :: Int -> [Int] -> [Int]
solve k xs = let groups = groupBy (\(a,_) (b,_) -> a==b) $ sortBy (comparing fst) $ zip xs [0..]
                 recurring = map head $ filter (\ys -> length ys >= k) groups
              in map fst $ sortBy (comparing snd) recurring
