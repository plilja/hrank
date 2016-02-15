import Text.Printf

main = do [n, m] <- fmap (map read . words) getLine
          let bubbles = n * m
          printf "%.3f\n" $ solve bubbles

solve :: Double -> Double
solve bubbles = sum $ [bubbles/i | i <- [1..bubbles]]
