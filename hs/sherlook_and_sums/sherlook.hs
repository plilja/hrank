import Control.Monad
import Control.Applicative
import Data.List

main = do t <- readLn
          replicateM_ t $ do
              getLine --ignore
              xs <- map read . words <$> getLine
              if solve xs
                 then putStrLn "YES"
                 else putStrLn "NO"

solve :: [Int] -> Bool
solve xs = let accFromLeft = accSum 0 xs
               accFromRight = reverse $ accSum 0 $ reverse xs
            in any (\(a, b) -> a == b) $ zip accFromLeft accFromRight

accSum _ [] = []
accSum acc (x:xs) = (x+acc):accSum (x+acc) xs
