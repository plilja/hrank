import Control.Applicative

pascals' 1 prevRow = [prevRow]
pascals' k prevRow = let prevRow' = [0] ++ prevRow ++ [0]
                         nextRow = zipWith (+) prevRow' (tail prevRow')
                      in prevRow:pascals' (k - 1) nextRow

pascals :: Int -> [[Int]]
pascals k = pascals' k [1]

printRow row = putStrLn (unwords $ map show row)

main = do k <- read <$> getLine
          let triangle = pascals k
          mapM_ printRow triangle
