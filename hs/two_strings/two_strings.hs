import Control.Monad
import Data.List

main = do t <- readLn
          replicateM_ t $ do
             as <- getLine
             bs <- getLine
             if solve as bs
                then putStrLn "YES"
                else putStrLn "NO"

solve as bs = let aChars = nub $ sort as
               in any (`elem` aChars) bs
