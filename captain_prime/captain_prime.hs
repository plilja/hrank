import Control.Applicative

digits :: Int -> [Int]
digits n = map (\x -> read (x:[]) :: Int) $ show n

fromDigits :: [Int] -> Int
fromDigits ds = read (concat $ map show ds) :: Int

isPrime 1 = False
isPrime n = let lim = floor $ sqrt $ fromIntegral n
             in foldl (\acc x -> acc && rem n x /= 0) True [2..lim]

isComposite = not . isPrime

lDecompIntoPrimes n = let ds = digits n
                          decomposition = map (\i -> fromDigits (drop i ds)) [0..length ds - 1]
                       in all isPrime decomposition

rDecompIntoPrimes n = let ds = digits n
                          decomposition = map (\i -> fromDigits (take i ds)) [1..length ds]
                       in all isPrime decomposition

doesNotContainZero n = not $ 0 `elem` (digits n)

determinePosition id
    | doesNotContainZero id && lDecompIntoPrimes id && rDecompIntoPrimes id = "CENTRAL"
    | doesNotContainZero id && lDecompIntoPrimes id && not (rDecompIntoPrimes id) = "LEFT"
    | doesNotContainZero id && not (lDecompIntoPrimes id) && rDecompIntoPrimes id = "RIGHT"
    | otherwise = "DEAD"

main = do ids <- map read . tail . lines <$> getContents
          let pos = map determinePosition ids
          mapM_ putStrLn pos
