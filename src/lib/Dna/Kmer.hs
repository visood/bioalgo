{-#LANGUAGE GADTs, StandaloneDeriving #-}
module Dna.Kmer where

import Data.Map (Map, (!))
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Dna.Dna

set :: Ord b => [b] -> Set b
set xs = Set.fromList xs

--hiddenMessage                      :: Base a =>  [a] -> [a]
--hiddenMessage text                 = "???"

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

  regionCovered    :: Base b => (a b) -> Int
  regionCovered c  = (maximum (poses c))  - (minimum (poses c)) + 1

  merge     :: (Base b, Cluster c) => (c b) -> (a b) -> (a b)
  absorb    :: Base b => Kmer b -> (a b) -> (a b)

data Clumer a where
  Clumer :: Base a => [a] -> Set Int -> Clumer a

deriving instance Show a => Show (Clumer a)
deriving instance Eq a => Eq (Clumer a)

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

mostFrequentKmers            :: Base a => Int -> [a] -> (Int, [[a]])
mostFrequentKmers k text     = (n, m ! n)
  where
    m = frequentKmers k text
    n = (maximum . M.keys) m

topFrequentKmers             :: Base a => Int -> Map [a] Int -> [([a], Int)]
topFrequentKmers n kcs       = take n (sortBy sortGT (M.toList kcs) )

sortGT (a1, b1) (a2, b2)
  | b1 < b2   = GT
  | b1 > b2   = LT
  | a1 < a2   = GT
  | otherwise = LT

frequentKmers                :: Base a => Int -> [a] -> Map Int [[a]]
frequentKmers k text         = invertedMap (kmerCounts k text)

invertedMap                  :: (Base a, Ord b) => Map [a] b -> Map b [[a]]
invertedMap                  = M.foldlWithKey (\m a b ->
                                                M.insertWith (++) b [a] m
                                              ) M.empty

kmerCounts                   :: Base a => Int -> [a] -> Map [a] Int
kmerCounts k text            = if (length kmer) < k
                               then M.empty
                               else M.insertWith (+) kmer 1 kcounts
  where
    kmer = take k text
    kcounts = kmerCounts k (drop 1 text)

kmerOccs                     :: Base a => Int -> [a] -> Map [a] [Int]
kmerOccs k text              = kmerOccsWithPos 0 k text
  where
    kmerOccsWithPos :: Base a => Int -> Int -> [a] -> Map [a] [Int]
    kmerOccsWithPos p k text = if (length kmer) < k
                               then M.empty
                               else M.insertWith (++) kmer [p] koccs
      where
        kmer = take k text
        koccs = kmerOccsWithPos (p + 1) k (drop 1 text)


clumers                     :: Base b => Int -> [b] -> [Clumer b]
clumers k tx                = map (\(s, xs) -> Clumer s (set xs)) (M.toList (kmerOccs k tx))

wordCounts                  :: Base a => [[a]] -> Map [a] Int
wordCounts words            = foldl (\m w -> M.insertWith (+) w 1 m) M.empty words

allKmers                    :: Base a => Int -> [a] -> [[a]]
allKmers k text             = take ((length text) - k + 1) (allKmers0 k text)

allKmers0                   :: Base a => Int -> [a] -> [[a]]
allKmers0 k text            = if null text
                              then []
                              else (take k text) : (allKmers0 k (drop 1 text))

ptrnCount                :: Base a =>  [a] -> [a] -> Int
ptrnCount _ []           = 0
ptrnCount [] _           = 0
ptrnCount ptrn text   = if (take l text == ptrn)
                              then 1 + n
                              else n
                              where
                                l = length ptrn
                                n = ptrnCount ptrn (drop 1 text)


isPrefix                    :: Base a => [a] -> [a] -> Bool
isPrefix [] _               = True
isPrefix _ []               = False
isPrefix (x:xs) (y:ys)      = (x == y) && (isPrefix xs ys)

occurences                  :: Base a =>  [a] -> [a] -> [Int]
occurences ptrn text     = occsWithPos 0 ptrn text
  where
    occsWithPos :: Base a => Int -> [a] -> [a] -> [Int]
    occsWithPos _ [] []        = []
    occsWithPos _ [] (y:_)     = []
    occsWithPos _ (x:_) []     = []
    occsWithPos p ptrn text = if (isPrefix ptrn text)
                                 then p : ns
                                 else ns
                                 where ns = occsWithPos (p + 1) ptrn (drop 1 text)

nextOcc :: (Base b, Show b) => [b] -> [b] -> Maybe Int
nextOcc [] _  = Nothing
nextOcc  _ [] = Nothing
nextOcc ptrn text = if (isPrefix ptrn text)
                       then Just 0
                       else case nextOcc ptrn (tail text) of
                              Nothing -> Nothing
                              Just q  -> Just (q + 1)

nextOccFrom                 :: (Base a, Show a) => Int -> [a] -> [a] -> Maybe Int
nextOccFrom  _  []    []    =  Nothing
nextOccFrom  _  []   (x:_)  =  Nothing
nextOccFrom  _ (x:_)  []    =  Nothing
nextOccFrom p ptrn  text =  if (isPrefix ptrn text)
                               then Just p
                               else nextOccFrom (p + 1) ptrn  (drop 1 text)

occurences2                 :: (Base a, Show a) => [a] -> [a] -> [Int]
occurences2 [] _            = []
occurences2 _ []            = []
occurences2 ptrn text    = case (nextOcc ptrn text) of
  Nothing -> []
  Just n  -> n : ( map (\p -> (n + 1 + p)) $ occurences2 ptrn (drop (n + 1) text))

{-
occurences2 ptrn text    = case (nextOccFrom 0 ptrn text) of
  Nothing -> []
  Just n  -> n : ( map (\p -> (n + 1 + p)) $ occurences2 ptrn (drop (n + 1) text))
-}

--kmerOccs                           :: Base a => Int -> [a] -> Map [a] [Int]
kmerClumps :: Base a => Int -> Int -> Int -> [a] -> Map [a] [[Int]]
kmerClumps k l t text = M.filter (\xs -> not (null xs))
                                 (M.map (\xs -> clumpInts l t xs) (kmerOccs k text))

runningCount :: (Base b, Show b) => [b] -> [b] -> [Int]
runningCount _ [] = []
runningCount [] text = take (length text) (repeat 0)
runningCount ptrn text = case (nextOccFrom 0 ptrn text) of
  Nothing -> take (length text) (repeat 0)
  Just n  -> (take n (repeat 0)) ++ (1:(map incr1 rest))
    where
      rest  = runningCount ptrn (drop (n + 1) text)
      incr1 = \c -> c + 1

--repeated patterns
isRepeated :: (Base b, Show b) => [b] -> [b] -> Bool
isRepeated xs ys
  | null ys              = null xs
  | null xs              = False
  | ly `mod` lx /= 0     = False
  | not (isPrefix xs ys) = False
  | ly == lx             = True
  | otherwise            = isRepeated xs (drop lx ys)
    where
      ly     = length ys
      lx     = length xs

repeatedPattern :: (Base b, Show b) => [b] -> [b]
repeatedPattern [] = []
repeatedPattern xs = prefixRepeat 1 xs
  where
    prefixRepeat :: (Base b, Show b) => Int -> [b] -> [b]
    prefixRepeat n xs
      | 2 * n > lx = xs
      | null xs = []
      | length xs < n = xs
      | otherwise = if isRepeated ys xs
                    then ys
                    else prefixRepeat (n + 1) xs
      where
        ys = take n xs
        lx = length xs
