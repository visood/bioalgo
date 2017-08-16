{-#LANGUAGE GADTs, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Genome.Data.Lexicographical where

import Data.Map (Map, (!))
import qualified Data.Map as M
import Util.Util (invertedMap, hamdist)


class LexOrd b where
  lexorder :: b -> Int
  lexvalue :: Int -> b


data Order b = Order { cardinality :: Int
                     , order :: b -> Int
                     , value :: Int -> b}

lexicord :: LexOrd b => Int -> Order b
lexicord k = Order k lexorder lexvalue

listOrder :: Ord b => Order b -> [b] -> Int
listOrder _ [] = 0
listOrder (Order k o v) (x:xs) = (o x) + k * (listOrder (Order k o v) xs)

listValue :: Ord b => Order b -> Int -> [b]
listValue _ 0 = []
listValue o x = (first o x) : (listValue o (rest o x))
  where
    first :: Ord b =>  Order b -> Int -> b
    first (Order k _ v) n = v (mod n k)
    rest  :: Ord b =>  Order b -> Int -> Int
    rest  (Order k _ _) n = div n k

{-
a list represented using its elements lexical ordering.
0 represents an empty list
-}
data OrdList a = OrdList { elemOrder :: Order a -- lexical order on elements
                         , repval :: Int }-- representative value

empty :: OrdList a -> Bool
empty (OrdList _ 0) = True
empty _             = False

hd :: OrdList a -> a --head
hd (OrdList (Order k _ v) x) = v (mod (x - 1) k)

tl :: OrdList a -> OrdList a --tail
tl (OrdList (Order k o v) x) = if x == 0
                                  then OrdList (Order k o v) 0
                                  else OrdList (Order k o v) ( div (x - 1) k)
