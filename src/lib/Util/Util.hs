module Util.Util where

import Data.Map (Map, (!))
import qualified Data.Map as M

invertedMap :: (Ord a, Ord b) => Map a b -> Map b [a]
invertedMap = M.foldlWithKey (\m x y ->
                               M.insertWith (++) y [x] m
                            ) M.empty
