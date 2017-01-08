{-#LANGUAGE GADTs, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Genome.Data.FrequencyArray where

import Data.List (sort)
import Data.Set (Set)
import Data.Sequence (Seq)
import Data.Sequence ((><), (<|), (|>))
import qualified Data.Foldable as Foldable
import Data.Map (Map, (!))
import qualified Data.Map as M
import Util.Util (invertedMap)
{-
To implement frequency arrays, we need a lexical order on types, and there
lists. Lexicographical order requires that the type be converted into an Int,
and an Int to the type.
-}
class Show b => Lexicord b where
  --cardinality :: Int, does not compile, needs b
  lexord      :: b -> Int
  lexval      :: Int -> b
  listlexord  :: Int -> [b] -> Int
  listlexord _ [] = 0
  listlexord k (x:xs) = 1 + (lexord x) + k * (listlexord k xs)
  listlexval  :: Int -> Int -> [b]
  listlexval _ 0 = []
  listlexval k x = (lexval $ mod rx k) : (listlexval k (div rx k))
    where rx = x - 1


mostFrequentKmers :: Lexicord b => Int -> Int -> [b] -> (Int, [[b]])
mostFrequentKmers c k text = (n, map (listlexval c)  (m ! n))
  where
    m = freqKmerLexicords c k text
    n = (maximum . M.keys) m    

freqKmerLexicords :: Lexicord b => Int -> Int -> [b] -> Map Int [Int]
freqKmerLexicords c k text = invertedMap (kmerLexicordCounts c k text)

kmerLexicordCounts :: Lexicord b => Int -> Int -> [b] -> Map Int Int
kmerLexicordCounts c k text = if (length kmer) < k
                         then M.empty
                         else M.insertWith (+) (listlexord c kmer) 1 kcounts
  where
    kmer = take k text
    kcounts = kmerLexicordCounts c k (drop 1 text)
