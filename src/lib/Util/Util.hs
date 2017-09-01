module Util.Util where

import Data.Map (Map, (!))
import qualified Data.Map as M

invertedMap :: (Ord a, Ord b) => Map a b -> Map b [a]
invertedMap = M.foldlWithKey (\m x y ->
                               M.insertWith (++) y [x] m
                            ) M.empty

hamdist :: (Eq b) => [b] -> [b] -> Int
hamdist [] [] = 0
hamdist [] ys = length ys
hamdist xs [] = length xs
hamdist (x:xs) (y:ys) = if x == y then rest else 1 + rest
  where rest = hamdist xs ys


cummsum :: (Foldable t, Num a) => t a -> [a]
cummsum xs = 0:(reverse $ snd $ foldl addHead (0, []) xs)
  where
    addHead = \(c, cs) x -> (c + x, (c + x):cs)

skew :: (Show b, Eq b) => b -> b -> [b] -> [Int]
skew x y text = cummsum $ map skvalue text
  where
    skvalue n
      | n == x    = 1
      | n == y    = -1
      | otherwise = 0

--find the minimum skew and positions where this minimum is achieved
--we will need to find the indexes in a list where elem has a given value
indexesWhere :: (Show a) => (a -> Bool) -> [a] -> [Int]
indexesWhere pred xs = indexesWhereWithAcc xs 0
  where
    indexesWhereWithAcc [] _ = []
    indexesWhereWithAcc (u:us) n = if pred u then n:nxt else nxt
      where nxt = indexesWhereWithAcc us (n+1)

minskew :: (Eq b, Show b) => b -> b -> [b] -> (Int, [Int])
minskew x y text = (mskew, indexesWhere (\u -> u == mskew) skewvals)
  where
    skewvals = skew x y text
    mskew    = minimum skewvals


maxskew :: (Eq b, Show b) => b -> b -> [b] -> (Int, [Int])
maxskew x y text = (mskew, indexesWhere (\u -> u == mskew) skewvals)
  where
    skewvals = skew x y text
    mskew    = maximum skewvals

  
