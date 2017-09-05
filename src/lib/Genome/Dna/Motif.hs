{-#LANGUAGE GADTs, StandaloneDeriving #-}
module Genome.Dna.Motif where
import Data.Map (Map, (!))
import Data.Set.Monad (Set)
import Data.Foldable (maximumBy, minimumBy)
import Data.Ord (comparing)
import qualified Data.Set.Monad as Set
import Genome.Dna.Dna
import Genome.Dna.Kmer
import Util.Util (hamdist)

--Haskell defines, so lets define
isMotif :: Base b => Int -> [[b]] -> [b] -> Bool
isMotif _ [] _ = False
isMotif d dna word = all containsSimilarWord dna
  where
    containsSimilarWord seq = any (\w -> hamdist w word <= d) (allKmers k seq)
    k = length word


--search all (k, d) motifs in a set of DNA sequences, exhaustively
enumeratedMotifs :: Base b => Int -> Int -> [[b]] -> Set [b]
enumeratedMotifs _ _ []        = Set.empty
enumeratedMotifs k d (seq:[])  = Set.fromList (allKmers k seq)
enumeratedMotifs k d (seq:dna) = Set.fromList $ filter (isMotif d dna) candidates
  where
    candidates = do
      candidate <- allKmers k seq
      flipped   <- Set.toList $ kmersWithinHamDist d candidate
      return flipped
{-
Implant a motif in a sequence at a given position
-}
implant :: Base b => [b] -> Int -> [b] -> [b]
implant word pos text = (take pos text) ++ word ++ (drop pos text)

--with 4 bases
type BaseVec t = (t, t, t, t)
{-
MotifMatrix is a matrix of dna sequences
-}

data MotifMatrix b = MotifMatrix {nrow :: Int
                                 ,ncol :: Int
                                 ,elems :: [[b]]}

instance Base b => Show (MotifMatrix b) where
  show (MotifMatrix nrow ncol ms) = (show nrow) ++ "\n" ++
                                    (show ncol) ++ "\n" ++
                                    (foldl
                                     (\s m -> s ++ (map symbol m) ++ "\n")
                                     ""
                                     ms)
--type MotifMatrix b = [[b]]

motifMatrix :: Base b => [[b]] -> MotifMatrix b
motifMatrix [] = MotifMatrix 0 0 []
motifMatrix motifs = MotifMatrix nrow ncol motifs
  where
    nrow = length motifs
    ncol = length $ head motifs

consMotif :: Base b => [b] -> MotifMatrix b -> MotifMatrix b
consMotif motif (MotifMatrix nr nc elems) = MotifMatrix (nr + 1) nc (motif:elems)

colhead :: MotifMatrix b -> [b]
colhead (MotifMatrix _ _ motifs) = map head motifs

coltail :: MotifMatrix b -> MotifMatrix b
coltail (MotifMatrix nr nc motifs) = MotifMatrix nr (nc - 1) (map tail motifs)

cols :: Base b => MotifMatrix b -> [[b]]
cols mm = if (ncol mm == 0) then [] else (colhead mm) : (cols $ coltail mm)

rowhead :: MotifMatrix b -> [b]
rowhead (MotifMatrix _ _ motifs) = head motifs

rowtail :: MotifMatrix b -> MotifMatrix b
rowtail (MotifMatrix nr nc motifs) = MotifMatrix (nr - 1) nc (tail motifs)

rows :: Base b => MotifMatrix b -> [[b]]
rows mm = elems mm

{-
For four nucleotides, we use 4-tuple for counting nucleotides in  motifs
-}
type CountMatrix   = [BaseVec Int]
type ProfileMatrix = [BaseVec Double]

profileMatrixFromRows :: [[Double]] -> ProfileMatrix
profileMatrixFromRows [[], [], [], []] = []
profileMatrixFromRows (as:cs:gs:ts:[]) = (head as, head cs, head gs, head ts):rest
  where rest = profileMatrixFromRows [tail as, tail cs, tail gs, tail ts]



getA :: BaseVec t -> t
getA (a, _, _, _) = a
getC :: BaseVec t -> t
getC (_, c, _, _) = c
getG :: BaseVec t -> t
getG (_, _, g, _) = g
getT :: BaseVec t -> t
getT (_, _, _, t) = t


getValueForBase :: Base b => b -> BaseVec t -> t
getValueForBase b (a, c, g, t) = case baseIndex b of
                                   0 -> a
                                   1 -> c
                                   2 -> g
                                   3 -> t

listOfZeros :: Int -> [Int]
listOfZeros k = take k (repeat 0)

sumByElem :: Num a => [a] -> [a] -> [a]
sumByElem [] _ = []
sumByElem _ [] = []
sumByElem (x:xs') (y:ys') = (x + y) : (sumByElem xs' ys')

baseCount :: Base b => [b] -> BaseVec Int
baseCount bs = foldl 
               (\(na, nc, ng, nt) b -> case baseIndex b of
                                         0 -> (na + 1, nc, ng, nt)
                                         1 -> (na, nc + 1, ng, nt)
                                         2 -> (na, nc, ng + 1, nt)
                                         3 -> (na, nc, ng, nt + 1))
               (0, 0, 0, 0)
               bs
               


motifsBaseCount :: Base b => b -> Int -> [[b]] -> [Int]
motifsBaseCount b k ms = foldl sumByElem (listOfZeros k) $ map (baseOccs b) ms
  where
    baseOccs b = map $ \x -> if (x == b) then 1 else 0

countMatrix :: Base b => MotifMatrix b -> CountMatrix
countMatrix mm = if (ncol mm == 0)
                 then []
                 else (baseCount $ colhead mm) : (countMatrix $ coltail mm)
  
  
profileMatrix :: Base b => MotifMatrix b -> ProfileMatrix
profileMatrix mm = map normedTuple $ countMatrix mm
  where
    normedTuple (na, nc, ng, nt) = (normed na, normed nc, normed ng, normed nt)
    normed n = (fromIntegral n :: Double) / numberSeqsD
    numberSeqsD = (fromIntegral (nrow mm) :: Double)
  
   

baseWithMaxOccurence :: BaseVec Double -> Int
baseWithMaxOccurence (a, c, g, t) = snd $ maximumBy (comparing fst) withBaseIndex
  where withBaseIndex = [(a, 0), (c, 1), (g, 2), (t, 3)]


consensusMotif :: Base b => MotifMatrix b -> [b]
consensusMotif mm = maxOccBaseSeq (profileMatrix mm)
  where
    maxOccBaseSeq :: Base b => [BaseVec Double] -> [b]
    maxOccBaseSeq [] = []
    maxOccBaseSeq ((pa, pc, pg, pt) : bvs') = b : maxOccBaseSeq bvs'
      where b = indexed $ baseWithMaxOccurence (pa, pc, pg, pt)

log4 :: Double -> Double
log4 x = log x / log 4

xlog :: Double -> Double -> Double
xlog _ 0 = 0::Double
xlog b x = x * (log x / log b)

xlog4 :: Double -> Double
xlog4 = xlog 4

xlog2 :: Double -> Double
xlog2 = xlog 2

baseEntropy :: BaseVec Double -> Double
baseEntropy (a, c, g , t)= - (xlog2 a + xlog2 c + xlog2 g + xlog2 t)

baseEntropyByElems :: [BaseVec Double] -> [Double]
baseEntropyByElems [] = []
baseEntropyByElems (xs:xss') = (baseEntropy xs):(baseEntropyByElems xss')


columnEntropy :: Base b => MotifMatrix b -> [Double]
columnEntropy mm = baseEntropyByElems $ profileMatrix mm

totalEntropy :: Base b => MotifMatrix b -> Double
totalEntropy mm = foldl (\s e -> s + e) 0.0 $ columnEntropy mm

distanceOfPattern :: Base b => [b] -> MotifMatrix b -> Int
distanceOfPattern word mm = foldl (\d r -> d + (hamdist word r)) 0 $ rows mm


distanceFromDna :: Base b => [[b]] -> [b] -> Int
distanceFromDna dna word = foldl (\d text -> d + (dseq text)) 0 dna
  where dseq text = distanceFromSeq text word 

distanceFromSeq :: Base b => [b] -> [b] -> Int
distanceFromSeq text word = minimum $ map (hamdist word) candidates
  where candidates = allKmers (length word) text

motifForPattern :: Base b => [b] -> [b] -> [b] --([b], Int)
motifForPattern word text  = fst $ minimumBy (comparing snd) kmersInSeq
  where
    kmersInSeq = map (\xs -> (xs, hamdist word xs)) $ allKmers k text
    k          = length word
    
  
medianString :: Base b => Int ->  [[b]] -> [b]
medianString k dna = fst $ minimumBy (comparing snd) $
                     map (\word -> (word, dis word)) $ kmerPatterns k
  where dis = distanceFromDna dna


kmerProbability :: Base b => ProfileMatrix -> [b] -> Double
kmerProbability pm bs = exp sumlogs
  where sumlogs = foldl (\s l -> s + l) 0.0 $
                  map (\(b, p) -> (log $ getValueForBase b p)) $ zip bs pm
                        

mostProbableMotif :: Base b => Int -> ProfileMatrix -> [b] -> [b]
mostProbableMotif k pm text = fst $ maximumBy (comparing snd) kps
  where
    kps = map (\km -> (km, kmerProbability pm km)) $ allKmers k text

greedilySearchedMotif :: Base b => Int -> [[b]] -> MotifMatrix b
greedilySearchedMotif _ [] = MotifMatrix []
greedilySearchedMotif k (seq0:[]) = MotifMatrix 1 k ([take k seq0])
greedilySearchedMotif k (seq0:remSeqs) =
  fst $ minimumBy (comparing snd) ( map (\mm -> (scoreMotifMatrix dna mm, mm))
                                    greedilyGrownFrom (allKmers k seq0) $
                                    
                                     
  where
    greedilyGrownFrom kmerSeeds (lastSeq:[]) = do
      seed <- kmerSeeds
      let pm = profileMatrix $ MotifMatrix 1 k [seed]
      return $ mostProbableMotif k pm lastSeq

    greedilyGrownFrom kmerSeeds (seqNext:otherSeqs) =
      

greedilySearchedMotif k (seq0:remSeqs) = consMotif motif0 motifsOfRemSeqs
  where
    motif0          = mostProbableMotif k pm seq0
    pm              = profileMatrix motifsOfRemSeqs
    motifsOfRemSeqs = greedilySearchedMotif k remSeqs
