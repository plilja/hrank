import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.ByteString.Lazy.Char8 as C8

solve :: [Integer] -> M.Map Integer Integer
solve xs = let xs' = L.sort xs
               (_, _, m) = foldr f (0, 0, M.empty) xs'
            in m
        where
            f x (s, i, acc) = (s + x, i + 1, (M.insert (s + x) (i + 1) acc))

lookupAns m s = case M.lookupGE s m of
                   Nothing -> C8.pack "-1"
                   (Just (_, v)) -> C8.pack (show v)

readInt bs = fst $ fromJust $ C8.readInteger bs

main = do (_:as:_:qs) <- C8.lines <$> C8.getContents
          let m = solve (map readInt (C8.words as))
          let answers = map (lookupAns m) $ (map readInt qs)
          mapM_ C8.putStrLn answers

