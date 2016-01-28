import Control.Applicative
import Data.List

main = do [n, money] <- map (read :: String -> Int) . words <$> getLine
          toys <- map (read :: String -> Int) . words <$> getLine
          let f money toys | null toys = 0
                           | head toys <= money = 1 + f (money - head toys) (tail toys)
                           | otherwise = 0
          let r = f money (sort toys)
          print r
