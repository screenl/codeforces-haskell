import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.Maybe
import qualified Data.Array           as A
import Data.Semigroup

data SegTree a =
    Node {
      val                   :: a
    , lazy                  :: Maybe a
    , left, right           :: Int
    , leftChild, rightChild :: SegTree a
    } |
    Leaf {
      val         :: a
    , lazy        :: Maybe a
    , left, right :: Int
    } deriving (Show, Eq, Ord)

(*.) :: Monoid a => Maybe a -> Maybe a -> Maybe a
(*.) = liftA2 mappend

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

instance Semigroup Int where
  (<>) = (+)
  stimes = (*) . fromIntegral

instance Monoid Int where 
  mempty = 0

instance Semigroup Integer where
  (<>) = max
  stimes _ = id

instance Monoid Integer where
  mempty = 0

initTree :: (Monoid a) => [a] -> SegTree a
initTree xs = aux 0 (n - 1)
    where
      vs = A.listArray (0,length xs-1) xs
      n = length vs
      aux l r
            | l == r =
                Leaf { val = vs A.! l, lazy = Nothing, left = l, right = r}
            | otherwise =
                let mid = (l + r) `div` 2
                    lChild = aux l mid
                    rChild = aux (mid + 1) r
                in Node { val = val lChild <> val rChild
                        , left = l
                        , right = r
                        , lazy = Nothing
                        , leftChild = lChild, rightChild = rChild
                        }



updateNode :: (Eq a, Monoid a) => SegTree a -> SegTree a
updateNode rt =
    if isNothing (lazy rt) then
        rt
    else
        let (lc, rc) = (leftChild rt, rightChild rt)
            (l, r) = (left rt, right rt)
        in if l == r then
               rt { val = val rt <> fromJust (lazy rt), lazy = Nothing }
           else
               let nlc = lc {lazy = lazy lc *. lazy rt}
                   nrc = rc {lazy = lazy rc *. lazy rt}
               in rt { val = val rt <> stimes (fI (r - l +1)) (fromJust (lazy rt))
                     , leftChild = nlc
                     , rightChild = nrc
                     , lazy = Nothing
                     }



queryTree :: (Eq a, Monoid a) => SegTree a -> Int -> Int -> (a, SegTree a)
queryTree root l r
    | l > r || r < left root || l > right root = (mempty, root)
    | otherwise =
        let nr = updateNode root
        in if left nr >= l && right nr <= r then
               (val nr, nr)
           else
               (fst (queryTree (leftChild nr) l r) <>
                fst (queryTree (rightChild nr) l r), nr)


updateTree :: (Eq a, Monoid a) => SegTree a -> Int -> Int -> a -> SegTree a
updateTree root l r inc
    | l > r = root
    | otherwise = aux nr
    where
      nr = updateNode root
      aux root
          | right root < l || left root > r = root
          | left root >= l && right root <= r =
              let lc = leftChild root
                  rc = rightChild root
                  [a, b] = [left root, right root]
              in
                if a /= b then
                    root { val = val root <> stimes (fI (b - a + 1)) inc
                         , leftChild = lc { lazy = lazy lc *. Just inc}
                         , rightChild = rc { lazy = lazy rc *. Just inc }
                         }
                else
                    root { val = val root <> stimes (fI (b - a + 1)) inc}
          | otherwise =
              let nlc = updateTree (leftChild root) l r inc
                  nrc = updateTree (rightChild root) l r inc
              in root { val = val nlc <> val nrc
                      , leftChild = nlc
                      , rightChild = nrc
                      }


