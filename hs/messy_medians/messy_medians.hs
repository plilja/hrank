import Data.Maybe
import qualified Data.Map as M

type Median = (MultiSet, MultiSet)

type MultiSet = (M.Map Int Int, Int)

insert :: Int -> MultiSet -> MultiSet
insert x (ms, count) = (M.insertWith (+) x 1 ms, count + 1)

delete :: Int -> MultiSet -> MultiSet
delete x (ms, count) = let newMap = M.updateWithKey (\k a -> if a == 1 then Nothing else Just (a - 1)) x ms
                        in (newMap, count - 1)

findMax :: MultiSet -> Int
findMax (ms, _) = fst $ M.findMax ms

findMin :: MultiSet -> Int
findMin (ms, _) = fst $ M.findMin ms


size :: MultiSet -> Int
size (_, count) = count

empty :: MultiSet
empty = (M.empty, 0)

median (left, right) = findMax left

balanceLeft (left, right) = if size left > size right + 1
                               then let y = findMax left
                                        newLeft = delete y left
                                     in (newLeft, insert y right)
                               else (left, right)

balanceRight (left, right) = if size left < size right
                                then let y = findMin right
                                         newRight = delete y right
                                     in (insert y left, newRight)
                                else (left, right)
balance :: Median -> Median
balance = balanceRight . balanceLeft

push :: Int -> Median -> Median
push x (left, right) = let isEmpty = (0 == size left)
                        in if isEmpty || x <= median (left, right)
                                then balance (insert x left, right)
                                else balance (left, insert x right)

run :: Int -> Int -> Median -> M.Map Int Median -> IO ()
run _ 0 _ _ = return ()
run i t currState states =
   do q <- readLn
      if q < 0
         then do let newState = fromJust $ M.lookup (i+q) states
                 print $ median newState
                 run (i + 1) (t - 1) newState (M.insert i newState states)
         else do let m' = push q currState
                 print (median m')
                 run (i + 1) (t - 1) m' (M.insert i m' states)

main = do t <- readLn
          run 1 t (empty, empty) M.empty

