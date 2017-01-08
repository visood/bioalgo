{-#LANGUAGE GADTs, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Genome.Data.FrequencyArray where

import Data.List (sort)
import Data.Set (Set)
import Data.Sequence (Seq)
import Data.Sequence ((><), (<|), (|>))
import qualified Data.Foldable as Foldable
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

