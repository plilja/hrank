import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Maybe

main = do
    [n, t] <- fmap (map readInt . BS.words) BS.getLine
    xs <- fmap (map readInt . BS.words) BS.getLine
    replicateM_ t $ do
        [i, j] <- fmap (map readInt . BS.words) BS.getLine
        BS.putStrLn $ BS.pack $ show $ minimum $ drop i $ take (j + 1) xs

readInt = fst . fromJust . BS.readInt
