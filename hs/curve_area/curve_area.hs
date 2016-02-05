import Text.Printf (printf)

eval :: [Int] -> [Int] -> Double -> Double
eval [] [] _ = 0
eval (a:as) (b:bs) x = (fromIntegral a) * (x**(fromIntegral b)) + (eval as bs x)

step = 0.001

takeStep x = fromIntegral (round (1000 * (x + step))) / 1000.0

type Func = (Double -> Double)

computeIntegral :: Double -> Double -> Func -> Double
computeIntegral left right f
    | left - 0.00000001 > right = 0
    | otherwise = step * (f (left)) + (computeIntegral (takeStep left) right f)

area :: Double -> Double -> [Int] -> [Int] -> Double
area left right as bs = computeIntegral left right (eval as bs)

volume :: Double -> Double -> [Int] -> [Int] -> Double
volume left right as bs = computeIntegral left right (\x -> pi * (eval as bs x)^2)

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = let ar = area (fromIntegral l) (fromIntegral r) a b
                    vol = volume (fromIntegral l) (fromIntegral r) a b
                 in [ar, vol]

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
