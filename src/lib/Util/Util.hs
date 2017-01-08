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
