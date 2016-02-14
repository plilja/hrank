import Data.Char

main = do [n, k] <- fmap words getLine
          print $ superDigit n (read k)

superDigit :: [Char] -> Integer -> Integer
superDigit [x] _ = read [x]
superDigit xs k = superDigit (digitSum xs k) 1

digitSum :: [Char] -> Integer -> [Char]
digitSum xs k = show $ (* k) $ sum $ map (read . (:[])) xs
