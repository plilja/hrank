import Control.Monad
import qualified Data.Map as M
import Data.Map ((!))
import Data.Char
import Data.Maybe

main = do
  [n, m] <- fmap (map read . words) getLine
  input <- fmap init getLine
  program <- fmap (filter (\c -> elem c "<>+-,.[]") . concat) $ replicateM m getLine
  putStrLn $ run program input

maxCount = 100000

run :: String -> String -> String
run ps is = let initialMemory = M.fromList $ zip [0..255] (repeat 0)
            in run' ps [] is initialMemory 0 0 []


run' :: String -> String -> String -> M.Map Int Int -> Int -> Int -> String -> String
run' [] disc is mem p count out = reverse out
run' ps disc is mem p count out | count >= maxCount = (reverse out) ++ "\nPROCESS TIME OUT. KILLED!!!"
                                | otherwise = run'' ps disc is mem p count out


run'' :: String -> String -> String -> M.Map Int Int -> Int -> Int -> String -> String
run'' ('>':ps) disc is mem p count out = run' ps ('>':disc) is mem ((p + 1) `mod` 256) (count + 1) out
run'' ('<':ps) disc is mem p count out = run' ps ('<':disc) is mem ((256 + p - 1) `mod` 256) (count + 1) out
run'' ('+':ps) disc is mem p count out = run' ps ('+':disc) is (M.insert p (((mem ! p) + 1) `mod` 256) mem) p (count + 1) out
run'' ('-':ps) disc is mem p count out = run' ps ('-':disc) is (M.insert p ((256 + (mem ! p) - 1) `mod` 256) mem) p (count + 1) out
run'' ('.':ps) disc is mem p count out = run' ps ('.':disc) is mem p (count + 1) (chr (mem ! p):out)
run'' (',':ps) disc (i:is) mem p count out = run' ps (',':disc) is (M.insert p (ord i) mem) p (count + 1) out
run'' ('[':ps) disc is mem p count out = if mem ! p == 0
                                            then run' (dropUntil ']' '[' ps 0) ((reverse $ takeUntil ']' '[' ps 0) ++ "[" ++ disc) is mem p (count + 2) out
                                            else run' ps ('[':disc) is mem p (count + 1) out
run'' (']':ps) disc is mem p count out = if mem ! p /= 0
                                            then run' ((reverse $ takeUntil '[' ']' disc 0) ++ "]" ++ ps) (dropUntil '[' ']' disc 0) is mem p (count + 1) out
                                            else run' ps (']':disc) is mem p (count + 1) out


dropUntil c d [] bal = []
dropUntil c d (x:xs) bal | c == x = if bal == 0 then xs else dropUntil c d xs (bal - 1)
                         | d == x = dropUntil c d xs (bal + 1)
                         | otherwise = dropUntil c d xs bal

takeUntil c d [] bal = []
takeUntil c d (x:xs) bal | c == x = if bal == 0 then [x] else x:takeUntil c d xs (bal - 1)
                         | d == x = x:takeUntil c d xs (bal + 1)
                         | otherwise = x:takeUntil c d xs bal

