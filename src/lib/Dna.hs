{-#LANGUAGE GADTs, StandaloneDeriving #-}
module Dna where

import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set

invalidPos :: Int
invalidPos = -1

class Ord a => Base a where
  bases       :: [a]
  indexed     :: Int -> a --to use in random generation
  complement  :: a -> a
  valid       :: a -> Bool
  valid x     = elem x bases
  symbol      :: a -> Char

instance Base Char where
  bases        = ['A', 'C', 'G', 'T', 'N', 'a', 'c', 'g', 't']
  indexed 0    = 'A'
  indexed 1    = 'C'
  indexed 2    = 'G'
  indexed 3    = 'T'
  indexed _    = 'N'
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

instance Base Int where
  bases        = [0, 1, 2, 3, 4]
  indexed i    = if (i < 0) then 4 else min i 4
  complement c = case c of
   0 -> 3
   1 -> 2
   2 -> 1
   3 -> 0
   4 -> 4
   _ -> c
  symbol c     = case c of
    0 -> 'A'
    1 -> 'C'
    2 -> 'G'
    3 -> 'T'

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
