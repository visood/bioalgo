{-#LANGUAGE FlexibleContexts #-}
module Test.Genome.Dna.Dna where

import Test.Util.Util
import Util.Util (hamdist)
import Genome.Dna.Dna
import Genome.Dna.Motif (isMotif, enumeratedMotifs)
import Genome.Dna.Variant

import Test.QuickCheck (Gen, choose, elements, generate,
                        listOf, listOf1, vectorOf, forAll)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Property(Property)

import Data.Set.Monad (member)

prop_idempotent_complement :: Base a => a -> Bool
prop_idempotent_complement c = complement (complement c) == c

prop_idempotent_revcomp    :: Base a => [a] -> Bool
prop_idempotent_revcomp cs = reverseComplement (reverseComplement cs) == cs

randomBase :: Base b => Gen b
randomBase = elements bases

randomBaseSequences :: Base b => Int -> Gen [b]
randomBaseSequences l = vectorOf l randomBase

testBaseSeqLen l = forAll (vectorOf l (randomBase :: Gen Char)) (hasLength l)
  where hasLength l seq = length seq == l

--DNA: n sequences of bases, length l each
randomDna :: Base b => Int -> Int -> Gen [[b]]
randomDna n l = vectorOf n (vectorOf l randomBase)
  
withInsert :: Base b => [b] -> Int -> [b] -> [b]
withInsert word pos seq = (take pos seq) ++ word ++ (drop pos seq)

randomDnaWithInsert :: Base b => Int -> Int -> [b] -> Gen [[b]]
randomDnaWithInsert numSeqs dnaLength insert = do
  dna <- randomDna numSeqs dnaLength
  pos <- choose (0,dnaLength)
  return $ map (withInsert insert pos ) dna

prop_dnaWithInsertLength = forAll
                           (randomDnaWithInsert 10 1000 "AAG")
                           (all (\seq -> length seq == 1003))

haveInsert :: String -> [String] -> Bool
haveInsert insert = all eachSeqContainsInsert
  where
    eachSeqContainsInsert :: String -> Bool
    eachSeqContainsInsert [] = False
    eachSeqContainsInsert seq = if (take $ length insert) seq == insert
                                then True
                                else eachSeqContainsInsert $ drop 1 seq

prop_dnaWithInsertHasMotif insert nseqs lseq = forAll dnaSeqs $ haveInsert insert
  where dnaSeqs = randomDnaWithInsert (makePos nseqs) ( makePos lseq) insert

--we want positive numbers for length and number of sequences
makePos num = 1 + (abs num)

prop_insertIsMotif :: String -> Int -> Int -> Int -> Property
prop_insertIsMotif insert dist nseqs lseq = forAll
                                            randomDnas
                                            (\seqs -> isMotif dist seqs insert)
  where
    randomDnas = randomDnaWithInsert (makePos nseqs) ( makePos lseq) insert

randomMutations seq = do
  number <- choose (0, length seq)
  poses  <- vectorOf number $ choose (0, length seq)
  muttos <- vectorOf number $ elements ['A', 'C', 'G', 'T']
  return $ zip poses muttos

prop_hamdistIsLessThanNumberOfMutations :: String -> Property
prop_hamdistIsLessThanNumberOfMutations seq = forAll
                                              (randomMutations seq)
                                              hamDistLessThanNumberOfMutations
  where
    hamDistLessThanNumberOfMutations muts = hamdist mutseq seq <= length muts
      where mutseq = mutateSeveralPos (map (\(p, m) -> (p, mutateTo m)) muts) seq



prop_mutatedInsertIsMotif :: Int -> Int -> Int -> Property
prop_mutatedInsertIsMotif insertLen nseqs lseq = forAll
                                                 randomDnas
                                                 insertIsMotif
  where
    randomDnas = do
      insert <- vectorOf (makePos insertLen) (elements ['A', 'C', 'G', 'T'])
      muttos <- randomMutations insert
      let mutatedInsert = mutateSeveralPos
                          (map (\(p, m) -> (p, mutateTo m)) muttos)
                          insert
      rdna <- randomDnaWithInsert (makePos nseqs) (makePos lseq) mutatedInsert
      return (insert, length muttos, rdna)

    insertIsMotif (insert, d, seqs) = isMotif d seqs insert
      

prop_enumeratedMotifsContainInsert :: Int -> Int -> Int -> Property
prop_enumeratedMotifsContainInsert insertLen nseqs lseq = forAll
                                                          randomDnas
                                                          insertIsEnumerated
  where
    randomDnas = do
      insert <- vectorOf insertLen' (elements ['A', 'C', 'G', 'T'])
      muttos <- randomMutations insert
      let mutatedInsert = mutateSeveralPos
                          (map (\(p, m) -> (p, mutateTo m)) muttos)
                          insert
      dnaWithMutatedInserts <- randomDnaWithInsert nseqs' lseq' mutatedInsert
      firstSeq <- vectorOf lseq' (elements ['A', 'C', 'G', 'T'])
      let firstSeqWithOriginalInsert = (withInsert insert 0 firstSeq)
          rdna = firstSeqWithOriginalInsert : dnaWithMutatedInserts
      return (insert, length muttos, rdna)
        where
          insertLen' = makePos insertLen
          nseqs'     = makePos nseqs
          lseq'       = makePos lseq

    insertIsEnumerated (insert, d, seqs) = member insert motifs
      where
        motifs = enumeratedMotifs (length insert) d seqs
      
                                                     

