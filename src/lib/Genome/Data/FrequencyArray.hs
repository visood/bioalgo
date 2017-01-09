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


  
mostFreqKmers :: Lexicord b => Int -> Int -> [b] -> (Int, [[b]])
mostFreqKmers c k text = (n, map (listlexval c)  (m ! n))
  where
    m = freqKmer c k text
    n = (maximum . M.keys) m

freqKmer :: Lexicord b => Int -> Int -> [b] -> Map Int [Int]
freqKmer c k text = invertedMap (kmerCounts c k text)

kmerCounts :: Lexicord a => Int -> Int -> [a] -> Map Int Int
kmerCounts c k text = if (length kmer) < k
                      then M.empty
                      else M.insertWith (+) (listlexord c kmer) 1 kcounts
  where
    kmer = take k text
    kcounts = kmerCounts c k (drop 1 text)


type Simpred b = [b] -> [b] -> Bool
type Simpats b = b -> [b]
type Cardinality = Int

mostSimFreqKmers :: Lexicord a => Cardinality -> Int -> [a] -> (Int, [a])
mostSimFreqKmers c k text = (n, map (listlexval c) (m ! n))
  where
    m = invertedMap $ (simKmerCounts c k text) :: Map Int Int
    n = (maximum . M.keys  m)

simKmerCounts :: Lexicord a, Sim b => Cardinality -> Int -> [a] -> Map b Int
simKmerCounts c k text = foldl
                         (\m p -> M.insert p (hcount p) m)
                         M.empty
                         (tpats >>= simpats)
  where
    kcs = kmerCounts c k text
    hcount p = foldl (\s q -> if aresim p q then s + (kcs ! q) else s) 0 tpats
    tpats = M.keys kcs


-- to order lists we need the cardinality of the alphabet
data LexicographicalOrder = Lexord Int
class Show b => Lexicordable b where
  lexord :: b -> Int
  lexval :: Int -> b
{-
mostSimFreqKmers :: Lexicord b => Int -> Simpred b -> Simpats Int -> Int -> [b] -> (Int, [[b]])
mostSimFreqKmers c aresim simpats k text = (n, map (listlexval c) (m ! n))
  where
    m = invertedMap (simKmerCounts c aresim simpats k text)
    n = (maximum . M.keys) m

simKmerCounts :: Lexicord b => Int -> Simpred b -> Simpats Int -> Int -> [b] -> Map Int Int
simKmerCounts c aresim simpats k text = foldl
                                        (\m p -> M.insert p (hcount p) m)
                                        M.empty
                                        (tpats >>= simpats)
  where
    kcs = kmerCounts c k text
    hcount p = foldl
               (\s q ->
                  if aresim (listlexval c p) (listlexval c q)
                  then s + (kcs ! q)
                  else s)
               0
               tpats
    tpats = M.keys kcs
-}
