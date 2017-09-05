{-#LANGUAGE GADTs, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Genome.Data.Lexicographical where

import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vec
import Data.List (sort)
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
data OrdList a = OrdList {elemOrder :: Order a -- lexical order on elements
                         ,repval :: Int }-- representative value

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



{-
our lexicographical order for a sequence assumes that
the order position increases along the sequence.
this assumption is useful because we do not need to specify the length of the
sequence to be encoded.
however, we may want to encode sequences in which the first element is in
the highest position.
this encoding will require the length of the sequence to decode.
-}

orderForPattern :: Order b -> [b] -> Int
orderForPattern o xs = listOrder o (reverse xs)

{-
@param element Order
@param coded value of the sequence
@param length of the sequence
@return decoded sequence
-}
patternForOrder :: Order b -> Int -> Int -> [b]
patternForOrder ob v n = reverse (revPatternForOrder v n)
  where
    revPatternForOrder v n
      | n == 0 = []
      | otherwise = first : (revPatternForOrder (div v k) (n - 1))
      where
        first = ((value ob) (mod v k))
        k = cardinality ob

patternSeq :: Order b -> Int -> [b] -> [Int]
patternSeq o k bs = patternSeqFromEncoded (ordseq o bs)
  where
    patternSeqFromEncoded :: [Int] -> [Int]
    patternSeqFromEncoded xs = if l == k then (first:rest) else []
      where
        l = length kelems
        first = (seqLexOrd (cardinality o) (reverse kelems)) - 1
        rest = patternSeqFromEncoded (drop 1 xs)
        kelems = take k xs
        
                         

patternFreqArray :: Order b -> Int -> [b] -> Vector Int
patternFreqArray o k bs = occs (patternSeq o k bs) (Vec.replicate n 0)
  where
    n = maxListOrder o k
    occs [] v = v
    occs (x:xs) v = occs xs (v // [(x, (v ! x) + 1)])


patternFreqPairs :: Order b -> Int -> [b] -> [(Int, Int)]
patternFreqPairs _ _ [] = []
patternFreqPairs o k (x:xs) = occs ((order o) x) 1 (sort $ patternSeq o k xs)
  where
    occs y c [] = [(y, c)]
    occs y c (z:zs) = if (z == y)
                      then occs y (c + 1) zs
                      else (y, c) : (occs z 1 zs)
