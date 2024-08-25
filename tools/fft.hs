import Data.Array.ST (runSTArray, newArray, readArray, writeArray, newListArray, STArray)
import Control.Monad (forM_, when)
import Data.Array (Array, assocs, bounds, listArray, Ix)
import Data.Array.Base ((!))
import Data.Complex
import GHC.Float (cosDouble, sinDouble)

modExp :: Int -> Int -> Int -> Int
modExp _ 0 _ = 1
modExp a b m
  | even b    = halfExp * halfExp `mod` m
  | otherwise = a * modExp a (b - 1) m `mod` m
  where halfExp = modExp a (b `div` 2) m

modInverse :: Int -> Int -> Int
modInverse a p = modExp a (p - 2) p

class Ring r where
  fromInt :: Int -> r
  zero :: r
  one :: r
  add :: r -> r -> r
  addi :: r -> r
  mul :: r -> r -> r

class (Ring f) => Field f where
  unityRoot :: Int -> f
  muli :: f -> f

instance Ring Int where
  fromInt = id
  zero = 0
  one = 1
  add x y = (x+y) `mod` 998244353
  mul x y = (x*y) `mod` 998244353
  addi x = 998244353 - x `mod` 998244353

instance Field Int where
  unityRoot k = modExp 3 (998244352 `div` k) 998244353
  muli x = modInverse x 998244353

instance Ring (Complex Double) where
  fromInt x = fromIntegral x :+ 0
  zero = 0 :+ 0
  one = 1 :+ 0
  add = (+)
  addi = (0-)
  mul = (*)

instance Field (Complex Double) where
  unityRoot k = cosDouble (2 * pi / fromIntegral k) :+ sinDouble (2 * pi / fromIntegral k)
  muli = (1/)

swapMArr arr x y = do
    xv <- readArray arr x
    yv <- readArray arr y
    writeArray arr x yv
    writeArray arr y xv

revArr n = runSTArray $ do
    rev <- newArray (0,n-1) 0
    forM_ [0..n-1] $ \i -> do
        prev <- (`div` 2) <$> readArray rev (i `div` 2)
        if odd i then
            writeArray rev i (half+prev)
        else writeArray rev i prev
    return rev
  where half = n `div` 2

rootCycle 0 _ = []
rootCycle k v = case rootCycle (k-1) v of
    a : l -> mul a v : a : l
    [] -> [one]

dftA :: Field f => [f] -> Array Int f
dftA xs = runSTArray $ do
    let rev = revArr $ length xs
    x <- newListArray (0,n-1) xs
    forM_ [0..n-1] $ \i -> do
        let r = rev ! i
        when (r < i) $ swapMArr x i r
    forM_ [2^x| x <- [1..62], 2^x <= n] $ \m -> do
        let k = m `div` 2
        let gn = unityRoot m
        let cycle = reverse $ rootCycle k gn
        forM_ [0,m..n-1] $ \i -> do
            forM_ (zip [0..k-1] cycle) $ \(j,g) -> do
                tmp <- mul g <$> readArray x (i+j+k)
                tmp2 <- readArray x (i+j)
                writeArray x (i+j+k) (add tmp2 $ addi tmp)
                writeArray x (i+j) (add tmp2 tmp)
    return x
  where n = length xs

dft :: Field f => [f] -> [f]
dft xs = map snd (assocs xt)
    where xt = dftA xs

idft :: Field f => [f] -> [f]
idft xs = map (mul inv) (a: reverse l)
  where n = fromInt $ length xs
        inv = muli n
        a:l = dft xs

maxPadSize :: Int -> Int
maxPadSize 0 = 0
maxPadSize n = 1 + maxPadSize (n `div` 2)

pad :: Ring a => [a] -> Int -> [a]
pad xs n = xs ++ replicate (n-length xs) zero

convolution :: Field f => [f] -> [f] -> [f]
convolution xs ys = idft $ zipWith mul xs' ys'
  where l = 2 ^ maxPadSize (length xs + length ys)
        xs' = dft $ pad xs l
        ys' = dft $ pad ys l

