module Kmers where

import Data.Map (Map, (!))
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Maybe
import Dna

--hiddenMessage                      :: Base a =>  [a] -> [a]
--hiddenMessage text                 = "???"

mostFrequentKmers                  :: Base a => [a] -> Int -> (Int, [[a]])
mostFrequentKmers text k           = (n, m ! n)
                                     where
                                       m = frequentKmers text k
                                       n = (maximum . M.keys) m

topFrequentKmers                   :: Base a => Int -> Map [a] Int -> [([a], Int)]
topFrequentKmers n kcs             = take n (sortBy sortGT (M.toList kcs) )

sortGT (a1, b1) (a2, b2)
  | b1 < b2   = GT
  | b1 > b2   = LT
  | a1 < a2   = GT
  | otherwise = LT

frequentKmers                      :: Base a => [a] -> Int -> Map Int [[a]]
frequentKmers text k               = invertedMap (kmerCounts k text)

invertedMap                        :: (Base a, Ord b) => Map [a] b -> Map b [[a]]
invertedMap                        = M.foldlWithKey (\m a b ->
                                                      M.insertWith (++) b [a] m
                                                    ) M.empty

kmerCounts                         :: Base a => Int -> [a] -> Map [a] Int
kmerCounts k text                  = if (length kmer) < k
                                     then M.empty
                                     else M.insertWith (+) kmer 1 kcounts
                                     where
                                       kmer = take k text
                                       kcounts = kmerCounts k (drop 1 text)

wordCounts                         :: Base a => [[a]] -> Map [a] Int
wordCounts words                   = foldl (\m w -> M.insertWith (+) w 1 m) M.empty words

allKmers                            :: Base a => [a] -> Int -> [[a]]
allKmers text k                     = take ((length text) - k + 1) (allKmers0 text k)

allKmers0                           :: Base a => [a] -> Int -> [[a]]
allKmers0 text k                    = if null text
                                      then []
                                      else (take k text) : (allKmers0 (drop 1 text) k)

patternCount                       :: Base a =>  [a] -> [a] -> Int
patternCount _ []                  = 0
patternCount [] _                  = 0
patternCount text pattern          = if (areMatchingPatterns (take l text) pattern)
                                     then 1 + n
                                     else n
                                     where
                                       l = length pattern
                                       n = patternCount (drop 1 text) pattern

areMatchingPatterns                ::  Base a => [a] -> [a] -> Bool
areMatchingPatterns [] []          = True
areMatchingPatterns (x:_) []       = False
areMatchingPatterns [] (y:_)       = False
areMatchingPatterns (x:xs) (y:ys)  = (x == y) && (areMatchingPatterns xs ys)


prefixes                           :: Base a => [a] -> [a] -> Bool
prefixes [] []                     = True
prefixes [] (y:_)                  = False
prefixes (x:_) []                  = True
prefixes (x:xs) (y:ys)             = (x == y) && (prefixes xs ys)

occurences                         :: Base a =>  [a] -> [a] -> [Int]
occurences text pattern            = occsWithPos 0 text pattern
  where
    occsWithPos _ [] []        = []
    occsWithPos _ [] (y:_)     = []
    occsWithPos _ (x:_) []     = []
    occsWithPos p text pattern = if (prefixes text pattern)
                                 then p : ns
                                 else ns
                                 where ns = occsWithPos (p + 1) (drop 1 text) pattern

nextOccFrom                         :: Base a => [a] -> Int -> [a] -> Maybe Int
nextOccFrom  []   _   []            =  Nothing
nextOccFrom  []   _  (x:_)          =  Nothing
nextOccFrom (x:_) _   []            =  Nothing
nextOccFrom text  p pattern         =  if (prefixes text pattern)
                                       then Just p
                                       else nextOccFrom (drop 1 text) (p + 1) pattern

occurences2                         :: Base a => [a] -> [a] -> [Int]
occurences2 [] []                   = []
occurences2 [] (y:_)                = []
occurences2 (x:_) []                = []
occurences2 text pattern            = case (nextOccFrom text 0 pattern) of
  Nothing -> []
  Just n  -> n : ( map (\p -> (n + 1 + p)) $ occurences2 (drop (n + 1) text) pattern)
