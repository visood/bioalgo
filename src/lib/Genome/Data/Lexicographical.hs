{-#LANGUAGE GADTs, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Genome.Data.Lexicographical where

import Data.Map (Map, (!))
import qualified Data.Map as M
import Util.Util (invertedMap, hamdist)


class LexOrd b where
  elemset  :: [b]
  lexorder :: b -> Int
  lexvalue :: Int -> b


data Order b = Order { cardinality :: Int
                     , order :: b -> Int
                     , value :: Int -> b}

lexicord :: LexOrd b => Int -> Order b
lexicord k = Order k lexorder lexvalue

listOrder :: Order b -> [b] -> Int
listOrder _ [] = 0
listOrder (Order k o v) (x:xs) = (o x) + k * (listOrder (Order k o v) xs)

{-
Max lexicographical order of a list
@param length of list
@return max order
-}
maxListOrder :: Order b -> Int -> Int
maxListOrder o 0 = 1
maxListOrder o n = (cardinality o) * (maxListOrder o (n - 1))

listValue :: Order b -> Int -> [b]
listValue _ 0 = []
listValue o x = (first o x) : (listValue o (rest o x))
  where
    first :: Order b -> Int -> b
    first (Order k _ v) n = v (mod n k)
    rest  :: Order b -> Int -> Int
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
{-
lexicographic representation of a list.
@param: a list of elements that can be Lexicographical ordered
@return: a list of ints representing the inputs' lexicographical order
-}
ordseq   :: Order b -> [b] -> [Int]
ordseq o = map (order o)
--seq :: LexOrd b => [b] -> [Int]
--seq = map lexorder

{-
given a cardinality, we can convert a sequence of intergers into a lexical order.
the sequence of intergers must be a sequence of lexicographical orders
@param cardinality::Int
@param sequence of lexicographical orders
@return compounded lexicographical order of a list
-}
seqLexOrd :: Int -> [Int] -> Int
seqLexOrd _ []     = 0
seqLexOrd k zs = 1 + (seqLexOrdNonEmptyList k zs)
  where
    seqLexOrdNonEmptyList _ []     = 0
    seqLexOrdNonEmptyList k (x:xs) = x + k * (seqLexOrdNonEmptyList k xs)

kmerSeq :: Order b -> Int -> [b] -> [Int]
kmerSeq o k bs = kmerSeqFromEncoded (ordseq o bs)
  where
    kmerSeqFromEncoded :: [Int] -> [Int]
    kmerSeqFromEncoded xs = if l == k
                            then firstKmer:(kmerSeqFromEncoded (drop 1 xs))
                            else []
      where
        l = length kelems
        firstKmer = seqLexOrd (cardinality o) kelems
        kelems = take k xs
