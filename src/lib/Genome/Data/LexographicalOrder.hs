{-#LANGUAGE GADTs, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Genome.Data.FrequencyArray where

import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Sequence (Seq)
import Data.Sequence ((><), (<|), (|>))
import qualified Data.Foldable as Foldable
import Data.Map (Map, (!))
import qualified Data.Map as M
import Util.Util (invertedMap, hamdist)


--data LexicographicalOrder = Lexord Int
data LexicographicalOrder b  = Lexord {cardinality :: Int
                                      , order :: Map b Int
                                      , value :: Map Int b}


lexord :: Show b => LexicographicalOrder b -> [b] -> Int
lexord _ [] = 0
lexord l (x:xs) = 1 + (l.order x) + (l.cardinality) * (lexord l xs)
lexval :: Show b => LexicographicalOrder b -> Int -> [b]
lexval _ 0 = []
lexval l x = (l.value (mod rx k)) : (lexval l (div rx k))
  where
    rx = x - 1
    k  = l.cardinality



--scratch below, fix and move elsewhere
mostFreqKmers :: LexicographicalOrder b -> Int -> [b] -> (Int, [b])
mostFreqKmers l k text = (n, map (l.lexval) (m ! n))
  where
    m = freqKmer l k text
    n = (maximum . M.keys) m

freqKmer :: LexicographicalOrder b -> Int -> [b] -> Map Int [Int]
freqKmer l k text = invertedMap (kmerCounts l k text)

kmerCounts :: LexicographicalOrder b -> Int -> [b] -> Map Int Int
kmerCounts l k text = if length kmer < k
                      then M.empty
                      else M.insertWith (+) (lexord l kmer) 1 kcounts
  where
    kmer = take k text
    kcounts = kmerCounts l k (drop 1 text)


simKmerCounts :: Simable b => LexicographicalOrder b -> Int -> [b] -> Map Int Int
simKmerCounts l k text = foldl
                         (\m p -> M.insert p (hcount p) m)
                         M.empty
                         (tpats >>= simpats)
  where
    kcs = kmerCounts l k text
    hcount p = foldl (\s q -> if aresim p q then s + (kcs ! q) else s) 0 tpats
    tpats = M.keys kcs

