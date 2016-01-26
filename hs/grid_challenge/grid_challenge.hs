import Control.Monad
import Data.List

main = do t <- readLn
          replicateM_ t $ do
             n <- readLn
             mss <- sequence $ replicate n getLine
             if solve mss
                then putStrLn "YES"
                else putStrLn "NO"

solve :: [[Char]] -> Bool
solve mss = let sorted = map sort mss
                transposed = transpose sorted
             in all isSorted transposed

isSorted xs = xs == (sort xs)
