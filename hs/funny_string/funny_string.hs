import Control.Applicative
import Data.Char (ord)

funny ss = let ss' = map ord ss
               rs' = reverse ss'
               ssDiffs = map abs $ zipWith (-) ss' (tail ss')
               rsDiffs = map abs $ zipWith (-) rs' (tail rs')
            in and $ zipWith (==) ssDiffs rsDiffs

main = do (_:input) <- lines <$> getContents
          let answers = map (\i -> if funny i then "Funny" else "Not Funny") input
          mapM_ putStrLn answers
