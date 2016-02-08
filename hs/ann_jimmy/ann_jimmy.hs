import Control.Applicative
import Control.Monad

main = do
    n <- (readLn :: IO Int)
    let side1 = ceiling $ (fromIntegral n) / 3
        side2 = ceiling $ (fromIntegral (n - side1)) / 2
        side3 = n - side1 - side2
        ans = side1 * side2 * side3
    print ans
