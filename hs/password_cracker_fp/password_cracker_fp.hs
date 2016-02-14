import Control.Monad
import qualified Data.Map as M
import Data.Map ((!))

main = do t <- readLn
          replicateM_ t $ do
              getLine
              passwords <- fmap words getLine
              loginAttempt <- getLine
              let ans = solve passwords loginAttempt
              putStrLn $ unwords ans


type AnsCache = M.Map Int (Bool, [String])

solve :: [String] -> String -> [String]
solve ps w = let ans = f ps w M.empty 0
              in snd $ ans ! 0


f :: [String] -> String -> AnsCache -> Int -> AnsCache
f _ [] c n = M.insert n (True, []) c
f ps w c n | M.member n c = c
           | otherwise = let cand = filter (startsWith w) ps
                             c' = foldl (combine ps w n) c cand
                          in case M.lookup n c' of
                               Nothing -> M.insert n (False, ["WRONG PASSWORD"]) c'
                               otherwise -> c'


combine :: [String] -> String -> Int -> AnsCache -> String -> AnsCache
combine ps w n c p = let pn = length p
                         c' = f ps (drop pn w) c (n + pn)
                      in case M.lookup (n + pn) c' of
                          Just (True, sol) -> M.insert n (True, (p:sol)) c'
                          otherwise -> c'


startsWith xs ys = let n = length ys
                    in ys == take n xs
