module Kmers where

import Data.Map (Map, (!))
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Maybe
import Dna

--hiddenMessage                      :: Base a =>  [a] -> [a]
--hiddenMessage text                 = "???"

mostFrequentKmers                  :: Base a => Int -> [a] -> (Int, [[a]])
mostFrequentKmers k text           = (n, m ! n)
                                     where
                                       m = frequentKmers k text
                                       n = (maximum . M.keys) m

topFrequentKmers                   :: Base a => Int -> Map [a] Int -> [([a], Int)]
topFrequentKmers n kcs             = take n (sortBy sortGT (M.toList kcs) )

sortGT (a1, b1) (a2, b2)
  | b1 < b2   = GT
  | b1 > b2   = LT
  | a1 < a2   = GT
  | otherwise = LT

frequentKmers                      :: Base a => Int -> [a] -> Map Int [[a]]
frequentKmers k text               = invertedMap (kmerCounts k text)

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

kmerOccs                           :: Base a => Int -> [a] -> Map [a] [Int]
kmerOccs k text                    = kmerOccsWithPos 0 k text
  where
    kmerOccsWithPos :: Base a => Int -> Int -> [a] -> Map [a] [Int]
    kmerOccsWithPos p k text = if (length kmer) < k
                               then M.empty
                               else M.insertWith (++) kmer [p] koccs
                               where
                                 kmer = take k text
                                 koccs = kmerOccsWithPos (p + 1) k (drop 1 text)

wordCounts                         :: Base a => [[a]] -> Map [a] Int
wordCounts words                   = foldl (\m w -> M.insertWith (+) w 1 m) M.empty words

allKmers                            :: Base a => Int -> [a] -> [[a]]
allKmers k text                      = take ((length text) - k + 1) (allKmers0 k text)

allKmers0                           :: Base a => Int -> [a] -> [[a]]
allKmers0 k text                    = if null text
                                      then []
                                      else (take k text) : (allKmers0 k (drop 1 text))

patternCount                       :: Base a =>  [a] -> [a] -> Int
patternCount _ []                  = 0
patternCount [] _                  = 0
patternCount pattern text          = if (areMatchingPatterns (take l text) pattern)
                                     then 1 + n
                                     else n
                                     where
                                       l = length pattern
                                       n = patternCount pattern (drop 1 text)

areMatchingPatterns                ::  Base a => [a] -> [a] -> Bool
areMatchingPatterns [] []          = True
areMatchingPatterns (x:_) []       = False
areMatchingPatterns [] (y:_)       = False
areMatchingPatterns (x:xs) (y:ys)  = (x == y) && (areMatchingPatterns xs ys)


isPrefix                           :: Base a => [a] -> [a] -> Bool
isPrefix [] _                      = True
isPrefix _ []                      = False
isPrefix (x:xs) (y:ys)             = (x == y) && (isPrefix xs ys)

occurences                         :: Base a =>  [a] -> [a] -> [Int]
occurences pattern text            = occsWithPos 0 pattern text
  where
    occsWithPos :: Base a => Int -> [a] -> [a] -> [Int]
    occsWithPos _ [] []        = []
    occsWithPos _ [] (y:_)     = []
    occsWithPos _ (x:_) []     = []
    occsWithPos p pattern text = if (isPrefix pattern text)
                                 then p : ns
                                 else ns
                                 where ns = occsWithPos (p + 1) pattern (drop 1 text)

nextOccFrom                         :: Base a => [a] -> Int -> [a] -> Maybe Int
nextOccFrom  []   _   []            =  Nothing
nextOccFrom  []   _  (x:_)          =  Nothing
nextOccFrom (x:_) _   []            =  Nothing
nextOccFrom pattern  p text         =  if (isPrefix pattern text)
                                       then Just p
                                       else nextOccFrom pattern (p + 1) (drop 1 text)

occurences2                         :: Base a => [a] -> [a] -> [Int]
occurences2 [] _                    = []
occurences2 _ []                    = []
occurences2 pattern text            = case (nextOccFrom pattern 0 text) of
  Nothing -> []
  Just n  -> n : ( map (\p -> (n + 1 + p)) $ occurences2 pattern (drop (n + 1) text))


--kmerOccs                           :: Base a => Int -> [a] -> Map [a] [Int]
kmerClumps :: Base a => Int -> Int -> Int -> [a] -> Map [a] [[Int]]
kmerClumps k l t text = M.filter (\xs -> not (null xs))
                                 (M.map (\xs -> Dna.clumpInts l t xs) (kmerOccs k text))
