{-#LANGUAGE GADTs #-}
module Dna where

class Ord a => Base a where
  allowed     :: [a]
  complement  :: a -> a
  valid       :: a -> Bool
  valid x     = elem x allowed
  symbol      :: a -> Char

instance Base Char where
  allowed      = ['A', 'C', 'G', 'T', 'N', 'a', 'c', 'g', 't']
  complement c = case c of
    'A' -> 'T'
    'a' -> 't'
    'C' -> 'G'
    'c' -> 'g'
    'G' -> 'C'
    'g' -> 'c'
    'T' -> 'A'
    't' -> 'a'
    'N' -> 'N'
    'n' -> 'n'
    _   ->  c

  symbol c = c


data Sense = Reverse | Forward
             deriving (Eq, Ord, Show, Read)

anti :: Sense -> Sense
anti Reverse = Forward
anti Forward = Reverse

reverseComplement :: Base a => [a] -> [a]

reverseComplement = foldl (\rs x -> (complement x):rs) []

isBase :: Char -> Bool
isBase c = c == 'A' || c == 'C' || c == 'G' || c == 'T'

isDNA :: String -> Bool
isDNA seq = all isBase  seq


class WithBaseSeq b where
  bseq :: Base a => (b a) -> [a]

class Located a where
  loc       :: a -> Int
  dis       :: a -> a -> Int
  dis x y   = abs ((loc x) - (loc y))

data Kmer a where
  Kmer :: Base a => [a] -> Int -> Kmer a

instance Base a => Eq (Kmer a) where
  (==) (Kmer s p) (Kmer r q) = s == r && p == q

instance Base a => Located (Kmer a) where
  loc (Kmer _ p) = p

instance WithBaseSeq Kmer where
  bseq (Kmer s _) = s

class Cluster a where
  locs      :: a -> [Int]
  leftmost  :: a -> Int
  leftmost a = if null xs
               then -1
               else head xs
    where xs = locs a
  rightmost :: a -> Int
  rightmost a = if null xs
                then -1
                else last xs
    where xs = locs a
  size      :: a -> Int
  size a    = length (locs a)

-- to use Prelude.length
instance Foldable Kmer where
  foldMap f (Kmer s _) = foldMap f s

data Clumer a where
  Clumer :: Base a => [a] -> [Int] -> Clumer a

instance Cluster (Clumer a) where
  locs (Clumer _ xs) = xs

instance WithBaseSeq Clumer where
  bseq (Clumer s _) = s

addClump :: Int -> [[Int]] -> Int -> [[Int]]
addClump l xss x = case xss of
  []     -> [[x]]
  xs:yss -> if (abs (x - (last xs))) <= l
            then xs:(x:xs):rest
            else xs:rest
            where rest = addClump l yss x

clumps :: Int -> Int -> [Int] -> [[Int]]
clumps l t xs = [ys | ys <- (foldl (addClump l) [] xs), length ys >= t]
