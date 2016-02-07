import Control.Monad
import Data.Maybe
import qualified Data.Map as M

main = do t <- readLn
          replicateM_ t $ do
             _ <- getLine
             xs <- fmap (map read . words) getLine
             if solve xs
                then putStrLn "Alice"
                else putStrLn "Bob"

solve :: [Int] -> Bool
solve xs = fst $ solve' xs M.empty

solve' :: [Int] -> M.Map [Int] Bool -> (Bool, M.Map [Int] Bool)
solve' xs memo | M.member xs memo = (fromJust (M.lookup xs memo), memo)
               | isIncreasingSeq xs = (False, M.insert xs False memo)
               | otherwise = let moves = [take (i-1) xs ++ drop i xs | i <- [1..length xs]]
                                 (invAns, newMemo) = foldl (\(acc, m) ys -> if (not acc) then (acc, m) else solve' ys m) (True, memo) moves
                                 ans = not invAns
                              in (ans, M.insert xs ans newMemo)


isIncreasingSeq xs = and $ map (\(a, b) -> a <= b) $ zip xs (tail xs)

