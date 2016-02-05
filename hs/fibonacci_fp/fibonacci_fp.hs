import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed
import Control.Applicative

m = 10^8 + 7
upperLim = 10^4

fibs :: UArray Int Int
fibs = runSTUArray $ do
         arr <- newArray (0, upperLim) 0
         writeArray arr 1 1
         forM_ [2..upperLim] $ \i -> do
             a <- readArray arr (i - 2)
             b <- readArray arr (i - 1)
             writeArray arr i ((a+b) `mod` m)
         return arr

main = do let arr = fibs
          t <- readLn
          replicateM_ t $ do
             q <- readLn
             let r = arr ! q
             print r

