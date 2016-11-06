
type Point = (Int, Int)

data Triangle = Tr {topCorner :: Point, width :: Int}


divide 0 t _ = t
divide n t ((tlx, tly), w) = let wSub = w `div` 2
                                 ySub = tly + wSub
                                 xSub = tlx + (wSub `div` 2)
                                 blanks = [[] | ]
                                 tSub = [[t !! i  | i <- [tlx..tlx+w]] j <- [tly..tly+w]]

sierpinski :: Int -> [[Char]]
sierpinski n = let initial = [replicate u '_' ++ replicate (63- 2*u) '1' ++ replicate u '_' | u <- [31,30..0]]
                 in divide n initial ((0, 0), 63)


main = do n <- readLn
          let s = sierpinski n
          mapM_ putStrLn s
