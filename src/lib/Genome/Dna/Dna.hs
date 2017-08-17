{-#LANGUAGE GADTs, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Genome.Dna.Dna where

import Data.List (sort)
import Data.Set (Set)
import Data.Sequence (Seq)
import Data.Sequence ((><), (<|), (|>))
import qualified Data.Foldable as Foldable
import Test.QuickCheck (Gen, choose, elements, generate, vectorOf)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Util.Util (hamdist)
import qualified Genome.Data.Lexicographical as Lexico
 {-
import Genome.Data.FrequencyArray (Lexicord,
                                   lexord,
                                   lexval,
                                   listlexord,
                                   listlexval)
-}

{-
Ix is used to map a contiguous sub-range of values in type onto integers.
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
  bases         :: [a]
  indexed       :: Int -> a --to use in random generation
  baseIndex     :: a -> Int
  complement    :: a -> a
  invalidBase   :: a
  isValidBase   :: a -> Bool
  isValidBase x = elem x bases
  symbol        :: a -> Char

instance Base Char where
  bases        = ['A', 'C', 'G', 'T']
  indexed 0    = 'A'
  indexed 1    = 'C'
  indexed 2    = 'G'
  indexed 3    = 'T'
  indexed _    = 'N'

  baseIndex 'A'    = 0
  baseIndex 'C'    = 1
  baseIndex 'G'    = 2
  baseIndex 'T'    = 3
  baseIndex  _     = 4

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

  invalidBase = 'N'

  symbol c = c

instance Base Int where
  bases        = [0, 1, 2, 3]
  indexed i    = if (i < 0) then 4 else min i 4
  baseIndex i      = min i 4
  complement c = case c of
   0 -> 3
   1 -> 2
   2 -> 1
   3 -> 0
   4 -> 4
   _ -> c
  invalidBase = 4
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

newtype Nucleotide = Nuc {unNuc :: Word8} deriving
  (Eq, Ord, Enum, Ix, Storable)

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
  bases = [_A_, _C_, _G_, _T_]
  indexed 0 = _A_
  indexed 1 = _C_
  indexed 2 = _G_
  indexed 3 = _T_
  indexed _ = _N_

  baseIndex (Nuc 1) = 0
  baseIndex (Nuc 2) = 1
  baseIndex (Nuc 4) = 2
  baseIndex (Nuc 8) = 3
  baseIndex  _  = 4

  complement (Nuc 0)  = Nuc 0
  complement (Nuc 1)  = Nuc 8
  complement (Nuc 2)  = Nuc 4
  complement (Nuc 4)  = Nuc 2
  complement (Nuc 8)  = Nuc 1
  complement (Nuc 15) = Nuc 15
  complement x = x

  invalidBase = Nuc 15

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

newtype AsBase b = AsBase b deriving Show

instance (Base b) => Lexico.LexOrd (AsBase b)  where
  elemset             = map AsBase bases
  lexvalue i          = AsBase (indexed i)
  lexorder (AsBase x) = baseIndex x

baseSeq :: [b] -> [AsBase b]
baseSeq = map AsBase

--numberBases :: Base b => Int
--numberBases = length (bases::[b])

baseLexOrder :: Base b => Lexico.Order b
baseLexOrder = Lexico.Order 4 baseIndex indexed

charBaseLexOrd = baseLexOrder :: Lexico.Order Char
charBaseLexSeq = Lexico.ordseq charBaseLexOrd
  
intBaseLexOrd  = baseLexOrder :: Lexico.Order Int
intBaseLexSeq  = Lexico.ordseq intBaseLexOrd

nucBaseLexOrd  = baseLexOrder :: Lexico.Order Nucleotide
nucBaseLexSeq  = Lexico.ordseq nucBaseLexOrd

  

--baseLexicOrder :: Base b => Lexico.Order (AsBase b)
--baseLexicOrder = Lexico.lexicord 4
