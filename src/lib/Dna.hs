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

class Eq a => Located a where
  pos       :: a -> Int
  distance  :: a -> a -> Int
  distance x y = abs ((pos x) - (pos y))

data Kmer a where
  Kmer :: Base a => [a] -> Int -> Kmer a

sequence :: Base a => Kmer a -> [a]
sequence (Kmer seq p) = seq

position :: Base a => Kmer a -> Int
position (Kmer seq p) = p

instance Located (Kmer a) where
  pos k = position k
