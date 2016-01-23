import qualified Data.List as DL
import Data.Ord
import Control.Applicative
import Text.Printf

type Point = (Double, Double)

ccw :: Point -> Point -> Point -> Int
ccw (p1x, p1y) (p2x, p2y) (p3x, p3y) = floor $ signum $ (p2x - p1x) * (p3y - p1y) - (p2y - p1y) * (p3x - p1x)

cosWithX :: Point -> Point -> Double
cosWithX (x1, y1) (x2, y2) = let adj = (x2 - x1)
                                 opp = (y2 - y1)
                                 hyp = sqrt (adj**2 + opp**2)
                              in adj / hyp

convexHull :: [Point] -> [Point]
convexHull xs =
    let p:rest = DL.sortBy (comparing snd) xs
        byAngle = DL.sortBy (comparing (\x -> - cosWithX p x)) rest
     in f byAngle [p]
    where
      f [] acc = acc
      f (p2:ps) [p1] = f ps [p2, p1]
      f (p3:ps) (p2:p1:acc) = case ccw p1 p2 p3 of
                                 1 -> f ps (p3:p2:p1:acc)
                                 _ -> f (p3:ps) (p1:acc)

calcDist' :: [Point] -> Double
calcDist' [_] = 0
calcDist' ((x1, y1):(x2, y2):xs) = let d = sqrt ((x1 - x2)**2 + (y1 - y2)**2)
                                    in d + calcDist' ((x2, y2):xs)

calcDist xs = calcDist' (xs ++ [head xs])

solve = calcDist . convexHull

parsePoint s = let [x, y] = map read $ words s
                in (x, y)

main = do points <- map parsePoint . tail . lines <$> getContents
          let p:rest = DL.sortBy (comparing snd) points
          let r = solve points
          printf "%.1f\n" r
