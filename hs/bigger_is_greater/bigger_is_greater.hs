import Control.Monad
import qualified Data.Set as S
import qualified Data.List as L

main = do t <- readLn
          replicateM_ t $ do
            xs <- getLine
            let ys = solve xs
            if xs == ys
              then putStrLn "no answer"
              else putStrLn ys

solve xs = f (reverse xs) [] S.empty
  where
    f [] ys s = ys
    f (x:xs) ys s = case S.lookupGT x s of
                      Just y -> (reverse xs) ++ (y:L.sort (L.delete y (x:ys)))
                      otherwise -> f xs (x:ys) (S.insert x s)
