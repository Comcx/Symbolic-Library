

module Test where

import qualified Data.Complex as DC
import Data.List


--Quick Sort Method--
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where smaller = [y | y <- xs, y <= x]
        larger  = [y | y <- xs, y >= x]



circle :: n -> Int -> [n]
circle _ 0 = []
circle n t = n : circle n (t-1)



for :: Int -> (Int -> Bool) -> Bool
for 0 f = f 0
for n f | f n       = for (n-1) f
        | otherwise = False



--Tree Usage--
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq, Read)

setTree :: Ord a => a -> Tree a -> Tree a
setTree a EmptyTree = Node a EmptyTree EmptyTree
setTree a (Node n left right) | a == n = Node a left right
                              | a < n  = Node n (setTree a left) right
                              | a > n  = Node n left (setTree a right)





data Seq c = Void
           | Seq c ::> c
           | Seq c :* c

drip' :: String -> String
drip' s = filter (\x -> x /= '"') s

varSeq :: Seq c -> [c]
varSeq (seq ::> c) = [c]
varSeq (seq :* c) = c : varSeq seq

newSeq :: a -> Seq a
newSeq a = Void ::> a


instance (Show c) => Show (Seq c) where
  show (Void) = "?"
  show (seq ::> c) = show c
  show (seq :* c) = drip' $ show seq ++ " * " ++ show c


instance (Ord c) => Eq (Seq c) where
  seq1 == seq2 = (sort $ varSeq seq1) == (sort $ varSeq seq2)






data KSeq k c = k :~ Seq c

instance (Show k, Show c) => Show (KSeq k c) where
  show (k :~ seq) = show k ++ " * " ++ "(" ++ show seq ++ ")"

instance (Ord k, Ord c) => Eq (KSeq k c) where
  (k1 :~ seq1) == (k2 :~ seq2) = (k1 == k2) && (seq1 == seq2)




data Series k c = Nil
                 | Series k c ::+ KSeq k c
                 | Series k c :+ KSeq k c

newSeries :: KSeq k c -> Series k c
newSeries kseq = Nil ::+ kseq


instance (Show k, Show c) => Show (Series k c) where
  show Nil = "?"
  show (series ::+ kseq) = show kseq
  show (series :+ kseq)  = show series ++ " + " ++ show kseq




main :: IO ()
main = do
  print "2333"






