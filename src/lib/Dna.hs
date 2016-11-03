{-#LANGUAGE GADTs, StandaloneDeriving #-}
module Dna where

import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set

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

class Located a where
  loc       :: Base b => (a b) -> Int
  dis       :: Base b => Located c => (a b) -> (c b) -> Int
  dis x y   = abs ((loc x) - (loc y))

data Kmer a where
  Kmer :: Base a => [a] -> Int -> Kmer a

instance Base a => Eq (Kmer a) where
  (==) (Kmer s p) (Kmer r q) = s == r && p == q

instance Located Kmer where
  loc (Kmer _ p) = p

instance WithBaseSeq Kmer where
  bseq (Kmer s _) = s

-- to use Prelude.length
instance Foldable Kmer where
  foldMap f (Kmer s _) = foldMap f s

class Cluster a where
  element   :: Base b => (a b) -> [b]
  poses     :: Base b => (a b) -> Set Int
  leftmost  :: Base b => (a b) -> Int
  leftmost a = if null xs
               then invalidPos
               else Set.findMin xs
               where xs = poses a
  rightmost :: Base b => (a b) -> Int
  rightmost a = if null xs
                then invalidPos
                else Set.findMax xs
                where xs = poses a
  size      :: Base b => (a b) -> Int
  size a    = Set.size (poses a)

  merge     :: (Base b, Cluster c) => (c b) -> (a b) -> (a b)
  absorb    :: Base b => Kmer b -> (a b) -> (a b)

data Clumer a where
  Clumer :: Base a => [a] -> Set Int -> Clumer a

deriving instance Show a => Show (Clumer a)

instance Located Clumer where
  loc   (Clumer _  xs) = Set.findMin xs

instance Cluster Clumer where
  element (Clumer s  _) = s
  poses (Clumer _  xs) = xs
  merge c (Clumer s xs) = if element c == s
                          then Clumer s (Set.union (poses c) xs)
                          else Clumer s xs
  absorb (Kmer s x) (Clumer r ys) = if s == r
                                    then Clumer r (Set.insert x ys)
                                    else Clumer r ys

instance WithBaseSeq Clumer where
  bseq (Clumer s _) = s

permAddUpto :: Int -> [[Int]] -> Int -> [[Int]]
permAddUpto l xss x = case xss of
  []     -> [[x]]
  xs:yss -> if (abs (x - (last xs))) <= l
            then (x:xs):rest
            else xs:rest
            where rest = addClump l yss x

addClump :: Int -> [[Int]] -> Int -> [[Int]]
addClump _ [] x = [[x]]
addClump l xss x = case growClump l x xs [] of
  ([], ys)  -> ys:yss
  (zs, ys)  -> ys:xs:yss
  where
    xs = head xss
    yss = tail xss
    growClump :: Int -> Int -> [Int] -> [Int] -> ([Int], [Int])
    growClump l x us ys = case (us, ys) of
      (_, [])   -> growClump l x us [x]
      ([], ws)  -> ([], ws)
      (w:ws, _) -> if x - w <= l
                   then growClump l x ws (ys ++ [w])
                   else (us, ys)

clumpInts :: Int -> Int -> [Int] -> [[Int]]
clumpInts l t xs = filter (\ys -> length ys >= t)
                          (foldl (\xss x -> addClump l xss x) [] xs)

clumps :: Base b => Int -> Int -> Clumer b -> [Clumer b]
clumps l t (Clumer s xs) = map (\ys -> Clumer s (Set.fromList ys))
                               (clumpInts l t (Set.toList xs))

partionClump :: Int -> [Int] -> ([Int], [Int])
partionClump l ys = growClump l [] ys
  where
    growClump :: Int -> [Int] -> [Int] -> ([Int], [Int])
    -- assuming sorted xs ++ ys
    growClump l xs ys = case (xs, ys) of
      (_, [])     -> (xs, [])
      ([], y:zs)  -> growClump l [y] zs
      (x:_, y:vs) -> if y - x <= l
                     then growClump l (xs ++ [y]) vs
                     else (xs, ys)


gatherClumps :: Int -> [Int] -> [[Int]]
gatherClumps l xs = case partionClump l xs of
  ([], []) -> []
  (ys, []) -> [ys]
  (ys, zs) -> if endsWith (fst ws) ys
              then ys : (gatherClumps l (snd ws))
              else ys : (gatherClumps l (drop 1 xs))
              where ws = partionClump l (drop 1 xs)

endsWith :: [Int] -> [Int] -> Bool
endsWith ys xs = startsWith (reverse ys) (reverse xs)

startsWith :: [Int] -> [Int] -> Bool
startsWith ys xs = case (ys, xs) of
  ([], _)      -> True
  (_, [])      -> False
  (y:vs, x:us) -> if (y==x)
                  then startsWith vs us
                  else False
