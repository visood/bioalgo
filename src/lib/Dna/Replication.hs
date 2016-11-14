{-#LANGUAGE GADTs, StandaloneDeriving #-}
module Dna.Replication where

import Data.Map (Map, (!))
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Dna.Dna


cummsum :: (Foldable t, Num a) => t a -> [a]
cummsum xs = reverse $ foldl (\cs x -> ((head cs) + x):cs) [0] xs

skew :: (Base b, Show b) => b -> b -> [b] -> [Int]
skew x y text = cummsum $ map skvalue text
  where
    skvalue n
      | n == x    = 1
      | n == y    = -1
      | otherwise = 0
