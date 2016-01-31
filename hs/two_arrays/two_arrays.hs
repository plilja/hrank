import Control.Monad
import Control.Applicative
import Data.List

main = do t <- readLn
          replicateM_ t $ do
             [n, k] <- map read . words <$> getLine
             xs <- map read . words <$> getLine
             ys <- map read . words <$> getLine
             if solve xs ys k
                then putStrLn "YES"
                else putStrLn "NO"

solve :: [Int] -> [Int] -> Int -> Bool
solve xs ys k = let smallToLarge = sort xs
                    largeToSmall = reverse $ sort ys
              in all (>= k) (zipWith (+) smallToLarge largeToSmall)
