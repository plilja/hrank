import Control.Applicative

left [] _ = []
left (x:xs) [] = 0:left xs [x]
left (x:xs) (s:stack)
           | fst x >= fst s = left (x:xs) stack
           | otherwise = (snd s):left xs (x:s:stack)

right xs stack = reverse $ left (reverse xs) stack

solve :: [Integer] -> Integer
solve xs = let xsWithIdx = zip xs [1..]
               ls = left xsWithIdx []
               rs = right xsWithIdx []
            in maximum $ zipWith (*) ls rs

main = do getLine --ignore first
          nums <- map read . words <$> getLine
          print $ solve nums


