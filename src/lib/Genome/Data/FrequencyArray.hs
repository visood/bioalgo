{-#LANGUAGE GADTs, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Genome.Data.FrequencyArray where

import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Sequence (Seq)
import Data.Sequence ((><), (<|), (|>))
import qualified Data.Foldable as Foldable
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vec
import qualified Genome.Data.Lexicographical as Lexico
import Util.Util (invertedMap, hamdist)
{-
To implement frequency arrays, we need a lexical order on types, and their
lists. Lexicographical order requires that the type be converted into an Int,
and an Int to the type.

following for references, it is deprecated --- implemented under
Genome.Data.Lexicographical

class Show b => Lexicord b where
  --cardinality :: Int, does not compile, needs b
  lexicord      :: b -> Int
  lexicval      :: Int -> b
  listlexicord  :: Int -> [b] -> Int
  listlexicord _ [] = 0
  listlexicord k (x:xs) = 1 + (lexicord x) + k * (listlexicord k xs)
  listlexicval  :: Int -> Int -> [b]
  listlexicval _ 0 = []
  listlexicval k x = (lexicval $ mod rx k) : (listlexicval k (div rx k))
    where rx = x - 1
-}

kmerCounts :: Lexico.Order b -> Int -> [b] -> Vector Int
kmerCounts o k bs = occs (Lexico.kmerSeq o k bs) (Vec.replicate n 0)
  where
    n = 1 + (Lexico.maxListOrder o k)
    occs [] v = v
    occs (x:xs) v = occs xs (v // [(x, (v ! x) + 1)])


{-
mostFreqKmers :: Lexicord b => Int -> Int -> [b] -> (Int, [[b]])
mostFreqKmers c k text = (n, map (listlexicval c)  (m ! n))
  where
    m = freqKmer c k text
    n = (maximum . M.keys) m

freqKmer :: Lexicord b => Int -> Int -> [b] -> Map Int [Int]
freqKmer c k text = invertedMap (kmerCounts c k text)

kmerCounts :: Lexicord a => Int -> Int -> [a] -> Map Int Int
kmerCounts c k text = if (length kmer) < k
                      then M.empty
                      else M.insertWith (+) (listlexicord c kmer) 1 kcounts
  where
    kmer = take k text
    kcounts = kmerCounts c k (drop 1 text)

-}
