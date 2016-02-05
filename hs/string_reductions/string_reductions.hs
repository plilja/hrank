import qualified Data.Set as S

solve _ [] = []
solve s (x:xs)
      | x `S.member` s = solve s xs
      | otherwise = x:solve (S.insert x s) xs

main = do line <- getLine
          putStrLn $ solve S.empty line
