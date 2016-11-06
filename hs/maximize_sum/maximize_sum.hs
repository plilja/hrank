import Control.Monad
import Control.Applicative
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Maybe

type MultiSet = M.Map Integer Integer

main = do (t:inp) <- map readInteger' . C8.words <$> C8.getContents
          run (fromInteger t) inp

readInteger' :: C8.ByteString -> Integer
readInteger' = fst . fromJust . C8.readInteger

run :: Int -> [Integer] -> IO ()
run 0 _ = return ()
run t inp = do
  let ([n, m], inp') = splitAt 2 inp
      (nums, inp'') = splitAt (fromInteger n) inp'
      r = solve m nums
  C8.putStrLn (C8.pack (show r))
  run (t-1) inp''

insert :: Integer -> MultiSet -> MultiSet
insert x ms = M.insertWith (+) x 1 ms

delete :: Integer -> MultiSet -> MultiSet
delete x ms =  M.updateWithKey (\k a -> if a == 1 then Nothing else Just (a - 1)) x ms

fromList :: [Integer] -> MultiSet
fromList [] = M.empty
fromList (x:xs) = let tmp = fromList xs
                   in insert x tmp

lookupLT :: MultiSet -> Integer -> Integer -> Integer
lookupLT m k def = let r = M.lookupLT k m
                    in case r of
                       (Just (k, v)) -> k
                       _ -> def

solve :: Integer -> [Integer] -> Integer
solve m nums = let accSums = fromList $ accList nums m 0
                in findMax m accSums nums 0

accList :: [Integer] -> Integer -> Integer -> [Integer]
accList [] _ _ = []
accList (x:xs) m acc = let acc' = (acc + x) `mod` m
                        in acc':accList xs m acc'

findMax _ _ [] _ = 0
findMax m accSums (x:xs) acc = let bestStartingAtX = ((lookupLT accSums acc acc) - acc) `mod` m
                                   acc' = (acc + x) `mod` m
                                   subSol = findMax m (delete acc' accSums) xs acc'
                                in maximum [bestStartingAtX, subSol, acc']

