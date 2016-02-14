main = do
  getLine
  bunnies <- fmap (map read . words) getLine
  print $ foldl lcm (head bunnies) (tail bunnies)
