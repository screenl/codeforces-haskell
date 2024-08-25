import System.IO
import Control.Monad (replicateM, replicateM_)

query a b = do
    putStr $ "? " ++ show a ++ " " ++ show b ++ "\n"
    hFlush stdout
    getLine

solve i k = do
    t <- read <$> query i k
    if i==t then return i else solve t k

loop 1 = []
loop i = solve 1 i : loop (i-1)

output _ [] = do
    putStr "\n"
    hFlush stdout
output i (a:l) = do
    putStr $ show a ++ " " ++ show i ++ " "
    output (i-1) l

run = do
    i <- read <$> getLine
    x <- sequence $ loop i
    putStr "! "
    output i x

main = do
    t <- read <$> getLine
    replicateM t run