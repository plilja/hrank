main = do n <- getLine
          hs <- fmap (map read . words) getLine
          print $ solve hs

solve :: [Integer] -> Integer
solve hs = f (reverse hs) 0
        where
          f [] ans = ans
          f (h:hs) botEnergy = let prevBotEnergy = ceiling (fromInteger (botEnergy + h) / 2)
                                in f hs prevBotEnergy
