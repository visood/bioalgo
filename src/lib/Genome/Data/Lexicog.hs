{-#LANGUAGE GADTs, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Genome.Data.Lexicog where

import Data.Map (Map, (!))
import qualified Data.Map as M
import Util.Util (invertedMap, hamdist)

data Order b = Order { cardinality :: Int
                     , order :: Map b Int
                     , value :: Map Int b}
alphabet :: Order b -> [b]
alphabet (Order _ o _) = M.keys o


ord :: Ord b => Order b -> [b] -> Int
ord _ [] = 0
ord (Order k o v) (x:xs) = 1 + (o ! x) + k * (ord (Order k o v) xs)

list :: Ord b => Order b -> Int -> [b]
list _ 0 = []
list o x = (first o x) : (list o (rest o x))

first :: Ord b =>  Order b -> Int -> b
rest  :: Ord b =>  Order b -> Int -> Int

first (Order k _ v) n = v ! (mod (n - 1) k)
rest  (Order k _ _) n = div (n - 1) k



baseLexOrd = Order k bo bv
  where
    k = 4
    bo = M.fromList [('A', 0), ('C', 1), ('G', 2), ('T', 3)]
    bv = M.fromList [(0, 'A'), (1, 'C'), (2, 'G'), (3, 'T')]
firstBase = first baseLexOrd
restBase = rest baseLexOrd
            

data OrdList a = OrdList { n :: Int
                         , k :: Int
                         , elems  :: Map Int a}

hd :: OrdList a -> a
hd (OrdList v k e) = e ! (mod (v - 1) k)

tl :: OrdList a -> OrdList a
tl (OrdList v k e) = OrdList (div (v - 1) k) k e



