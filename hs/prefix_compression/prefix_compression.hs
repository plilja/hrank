import Text.Printf

solve [] ys = ([], [], ys)
solve xs [] = ([], xs, [])
solve (x:xs) (y:ys)
     | x == y = let (pre, xs', ys') = solve xs ys
                 in (x:pre, xs', ys')
     | otherwise = ([], x:xs, y:ys)

main = do xs <- getLine
          ys <- getLine
          let (pre, xs', ys') = solve xs ys
          printf "%d %s\n" (length pre) pre
          printf "%d %s\n" (length xs') xs'
          printf "%d %s\n" (length ys') ys'
