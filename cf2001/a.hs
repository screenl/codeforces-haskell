import Control.Monad (replicateM)
import Data.List (unfoldr, sort)

split chr = unfoldr sep where
  sep [] = Nothing
  sep l  = Just . fmap (drop 1) . break (== chr) $ l

sloop i occ m (a:l) = if i == a
    then sloop i (occ+1) m l
    else sloop a 1 (max occ m) l
sloop i occ m [] = max occ m

run [a,b] = read a - sloop 0 0 0 (sort (read <$> split ' ' b))

main = do
    t <- read <$> getLine
    l <- replicateM t (replicateM 2 getLine)
    mapM_ print (run <$> l)