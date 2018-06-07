

module Symbolic ( Fraction(..)
                , Liter(..)
                , Seq(..) 
                , Series(..)
                , reductFrac
                , newSeq
                , newSeries
                , mergeKSeq ) where


import qualified Data.Complex as DC
import Data.List




data Fraction = (:/) { up :: Int, down :: Int}

instance Eq (Fraction) where
  (a_up :/ a_down) == (b_up :/ b_down) = a_up * b_down == b_up * a_down

instance Show (Fraction) where
  show (up :/ down) = show up ++ if down == 1 then "" else (" / " ++ show down)

instance Ord (Fraction) where
  (a_up :/ a_down) <= (b_up :/ b_down) = a_up * b_down <= b_up * a_down


reductFrac :: Fraction -> Fraction
reductFrac frac = (upPart `div` divisor) :/ (downPart `div` divisor) where
  upPart   = up frac
  downPart = down frac
  divisor  = gcd (upPart) (downPart)





data Liter c = c :^ Fraction

instance (Eq c) => Eq (Liter c) where
  (c1 :^ frac1) == (c2 :^ frac2) = c1 == c2 && frac1 == frac2

instance (Show c) => Show (Liter c) where
  show (c :^ frac) | down frac == 1 = drip' $ show c ++ " ^ " ++ show frac
                   | otherwise      = drip' $ show c ++ " ^ " ++ "(" ++ show frac ++ ")"

instance (Ord c) => Ord (Liter c) where
  (c1 :^ frac1) <= (c2 :^ frac2) = (c1 == c2) && frac1 <= frac1




--------------- Seq ----------------

data Seq c = Void
           | Seq c ::> Liter c
           | Seq c :* Liter c

drip' :: String -> String
drip' s = filter (\x -> x /= '"') s

varSeq :: Seq c -> [Liter c]
varSeq (seq ::> c) = [c]
varSeq (seq :* c)  = c : varSeq seq

newSeq :: Liter a -> Seq a
newSeq a = Void ::> a


instance (Show c) => Show (Seq c) where
  show (Void) = "?"
  show (seq ::> c) = "(" ++ show c ++ ")"
  show (seq :* c)  = drip' $ show seq ++ " * " ++ "(" ++ show c ++ ")"


instance (Ord c) => Eq (Seq c) where
  seq1 == seq2 = (sort $ varSeq seq1) == (sort $ varSeq seq2)




--------------------- KSeq ----------------------

data KSeq c = Int :~ Seq c

k (k :~ _) = k
seq' (_ :~ seq) = seq

instance (Show c) => Show (KSeq c) where
  show (k :~ seq) = (if k == 1 then "" else show k ++ " * ") ++ "(" ++ show seq ++ ")"

instance (Ord c) => Eq (KSeq c) where
  (k1 :~ seq1) == (k2 :~ seq2) = (k1 == k2) && (seq1 == seq2)



------------------- Series ---------------------

data Series c = Nil
                 | Series c ::+ KSeq c
                 | Series c :+ KSeq c

newSeries :: KSeq c -> Series c
newSeries kseq = Nil ::+ kseq


instance (Show c) => Show (Series c) where
  show Nil = "?"
  show (series ::+ kseq) = show kseq
  show (series :+ kseq)  = show series ++ " + " ++ show kseq





merge :: (Ord c) => KSeq c -> Series c -> Series c
merge kseq Nil = Nil ::+ kseq
merge kseq (series ::+ x) | seq' x == seq' kseq = merge ((k kseq + k x) :~ seq' x) series
                          | otherwise           = merge kseq series :+ x
merge kseq (series :+  x) | seq' x == seq' kseq = merge ((k kseq + k x) :~ seq' x) series
                          | otherwise           = merge kseq series :+ x


mergeKSeq :: (Ord c) => Series c -> Series c
mergeKSeq Nil = Nil
mergeKSeq (series ::+ kseq) = merge kseq (mergeKSeq series)
mergeKSeq (series :+ kseq)  = merge kseq (mergeKSeq series)





----------------------------- test -----------------------------


ser = newSeries ((5 :~) $ newSeq ("a" :^ (2:/1)) :* ("b" :^ (3:/1))) :+ 
                ((2 :~) $ newSeq ("a" :^ (2:/1)) :* ("b" :^ (3:/1))) :+
                ((3 :~) $ newSeq ("c" :^ (2:/1)) :* ("d" :^ (1:/2)))












