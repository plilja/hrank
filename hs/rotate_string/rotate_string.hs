import Control.Applicative
import Data.List

rotations xs = map (\i -> drop i xs ++ take i xs) [1..length xs]

solve xs = do let rs = intersperse " " $ rotations xs
              mapM_ putStr rs
              putStrLn ""

main = do (_:xs) <- lines <$> getContents
          mapM_ solve xs
