import Control.Monad (replicateM)
import Data.List (unfoldr, sort)

printseq = foldr (\ a -> (>>) (putStr (show a) >> putStr " ")) (putStr "")

run i = if even i
then print (-1)
else printseq (filter odd [1..i]) >> printseq (filter even [1..i])>> putStr "\n"

main = do
    t <- read <$> getLine
    l <- replicateM t getLine
    mapM_ run (read <$> l)