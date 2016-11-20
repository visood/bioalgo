{-#LANGUAGE GADTs, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Dna.Dna where

import Data.List (sort)
import Data.Set (Set)
import Data.Sequence (Seq)
import Data.Sequence ((><), (<|), (|>))
import qualified Data.Foldable as Foldable
import Test.QuickCheck (Gen, choose, elements, generate, vectorOf)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

{-
Ix is used to map a contiguous subrange of values in type onto integers.
It is used primarily for array indexing (see the array package). Ix uses
row-major order.
-}
import Data.Ix
{-
Foreign.Storable provides most elementary support for marshalling and is part
of the language-independent portion of the Foreign Function Interface (FFI), and
will normally be imported via the Foreign module
-}
import Foreign.Storable
import qualified Data.Set as Set
import Data.Word

invalidPos :: Int
invalidPos = -1

class Ord a => Base a where
  bases       :: [a]
  indexed     :: Int -> a --to use in random generation
  complement  :: a -> a
  valid       :: a -> Bool
  valid x     = elem x bases
  symbol      :: a -> Char

instance Base Char where
  bases        = ['A', 'C', 'G', 'T', 'N', 'a', 'c', 'g', 't']
  indexed 0    = 'A'
  indexed 1    = 'C'
  indexed 2    = 'G'
  indexed 3    = 'T'
  indexed _    = 'N'
  complement c = case c of
    'A' -> 'T'
    'a' -> 't'
    'C' -> 'G'
    'c' -> 'g'
    'G' -> 'C'
    'g' -> 'c'
    'T' -> 'A'
    't' -> 'a'
    'N' -> 'N'
    'n' -> 'n'
    _   ->  c

  symbol c = c

instance Base Int where
  bases        = [0, 1, 2, 3, 4]
  indexed i    = if (i < 0) then 4 else min i 4
  complement c = case c of
   0 -> 3
   1 -> 2
   2 -> 1
   3 -> 0
   4 -> 4
   _ -> c
  symbol c     = case c of
    0 -> 'A'
    1 -> 'C'
    2 -> 'G'
    3 -> 'T'

data Sense = Reverse | Forward
             deriving (Eq, Ord, Show, Read)

anti :: Sense -> Sense
anti Reverse = Forward
anti Forward = Reverse

reverseComplement :: Base a => [a] -> [a]
reverseComplement = foldl (\rs x -> (complement x):rs) []

isBase :: Char -> Bool
isBase c = c == 'A' || c == 'C' || c == 'G' || c == 'T'

isDNA :: String -> Bool
isDNA seq = all isBase  seq


class WithBaseSeq b where
  bseq :: Base a => (b a) -> [a]

newtype Nucleotide = Nuc {unNuc :: Word8} deriving (
  Eq, Ord, Enum, Ix, Storable
  )

instance Bounded Nucleotide where
  minBound = Nuc 0
  maxBound = Nuc 15

instance Show Nucleotide where
  show (Nuc x) | x == 0    = "-"
               | x == 1    = "A"
               | x == 2    = "C"
               | x == 4    = "G"
               | x == 8    = "T"
               | x == 15   = "N"
               | otherwise = "X"

gap, _A_, _C_, _G_, _T_, _N_ :: Nucleotide
gap = Nuc 0
_A_ = Nuc 1
_C_ = Nuc 2
_G_ = Nuc 4
_T_ = Nuc 8
_N_ = Nuc 15

instance Base Nucleotide where
  bases = [_A_, _C_, _G_, _T_, _N_]
  indexed 0 = gap
  indexed 1 = _A_
  indexed 2 = _C_
  indexed 3 = _G_
  indexed 4 = _T_
  indexed _ = _N_
  complement (Nuc 0)  = Nuc 0
  complement (Nuc 1)  = Nuc 8
  complement (Nuc 2)  = Nuc 4
  complement (Nuc 4)  = Nuc 2
  complement (Nuc 8)  = Nuc 1
  complement (Nuc 15) = Nuc 15
  complement x = x
  symbol (Nuc 0)  = '-'
  symbol (Nuc 1)  = 'A'
  symbol (Nuc 2)  = 'C'
  symbol (Nuc 4)  = 'G'
  symbol (Nuc 8)  = 'T'
  symbol (Nuc 15) = 'N'
  symbol x        = 'X'

type DnaSeq = Seq Nucleotide

instance Arbitrary Nucleotide where
  arbitrary = do
    x <- choose (0, 3) :: Gen Int
    return (Nuc (2 ^ x))

dnaString :: (Base b) => [b] -> String
dnaString []     = ""
dnaString (b:bs) = (symbol b) : (dnaString bs)

randNuc0 = elements [gap, _A_, _C_, _G_, _T_]
randNuc  = elements [_A_, _C_, _G_, _T_]

randomDna :: Int -> Gen [Nucleotide]
randomDna k = vectorOf k randNuc

