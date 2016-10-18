module Kmers where

import Data.Map (Map, (!))
import Data.List (sortBy)
import qualified Data.Map as M

hiddenMessage                      :: String -> String
hiddenMessage text                 = "???"

mostFrequentKmers                  :: String -> Int -> (Int, [String])
mostFrequentKmers text k           = (n, m ! n)
                                     where
                                       m = frequentKmers text k
                                       n = (maximum . M.keys) m

topFrequentKmers                   :: Int -> Map String Int -> [(String, Int)]
topFrequentKmers n kcs             = take n (sortBy sortGT (M.toList kcs) )

sortGT (a1, b1) (a2, b2)
  | b1 < b2   = GT
  | b1 > b2   = LT
  | a1 < a2   = GT
  | otherwise = LT

frequentKmers                      :: String -> Int -> Map Int [String]
frequentKmers text k               = invertedMap (kmerCounts text k)

invertedMap                        :: Ord b => Map a b -> Map b [a]
invertedMap                        = M.foldlWithKey (\m a b ->
                                                      M.insertWith (++) b [a] m
                                                    ) M.empty

kmerCounts                         :: String -> Int -> Map String Int
kmerCounts text k                  = if (length kmer) < k
                                     then M.empty
                                     else M.insertWith (+) kmer 1 kcounts
                                     where
                                       kmer = take k text
                                       kcounts = kmerCounts (drop 1 text) k

wordCounts                         :: [String] -> Map String Int
wordCounts words                   = foldl (\m w -> M.insertWith (+) w 1 m) M.empty words

allKmers                            :: String -> Int -> [String]
allKmers text k                     = take ((length text) - k + 1) (allKmers0 text k)

allKmers0                           :: String -> Int -> [String]
allKmers0 text k                    = if null text
                                      then []
                                      else (take k text) : (allKmers0 (drop 1 text) k)

patternCount                       :: String -> String -> Int
patternCount _ []                  = 0
patternCount [] _                  = 0
patternCount text pattern          = if (areMatchingPatterns (take l text) pattern)
                                     then 1 + n
                                     else n
                                     where
                                       l = length pattern
                                       n = patternCount (drop 1 text) pattern

areMatchingPatterns                :: String -> String -> Bool
areMatchingPatterns [] []          = True
areMatchingPatterns (x:_) []       = False
areMatchingPatterns [] (y:_)       = False
areMatchingPatterns (x:xs) (y:ys)  = (x == y) && (areMatchingPatterns xs ys)
