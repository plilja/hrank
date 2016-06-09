import Control.Monad
import Data.Char

main = do
    (_:inp) <- fmap words getContents
    mapM_ print $ map solve inp

solve :: String -> Int
solve xs = let m = (length xs) `div` 2
               (left, right) = splitAt m xs
            in sum $ zipWith (\a b -> abs (ord a - ord b)) left (reverse right)
