{-#LANGUAGE FlexibleInstances #-}
module Test.Dna.Kmer where
import Dna.Kmer
import Dna.Dna
import Test.Util.Util
import Test.Dna.Dna
import qualified Data.Map as M
import Test.QuickCheck (Gen, choose, elements, generate,
                        listOf, listOf1, vectorOf)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

instance Arbitrary (Clumer Nucleotide) where
  arbitrary = do
    ps <- listOf $ choose (0,1000)
    sq <- listOf1 randNuc
    return (Clumer sq (set ps))

 {-
instance Arbitrary (Int, Int, Clumer Nucleotide) where
  arbitrary = do
    l <- choose (0, 100)
    t <- choose (1, 10)
    c <- arbitrary
    return (l, t, c)
-}

prop_seqKmerSize :: (Base b, Show b) => Int -> [b] -> Bool
prop_seqKmerSize k seq = all (\c -> length c == k) (M.keys $ kmerOccs k seq)

prop_clumerInSeq :: (Base b, Show b) => Int -> [b] -> Bool
prop_clumerInSeq k bs = all isat (clumers k bs)
  where
    isat = \c -> all (\x -> take k (drop x bs) == (element c)) (poses c)


prop_kmerOccInSeq :: (Base b, Show b) => Int -> [b] -> Bool
prop_kmerOccInSeq k seq = all occurence (M.toList $ kmerOccs k seq)
  where
    occurence (cseq, xs) = all (\x -> take k (drop x seq) == cseq) xs

{-
prop_textKmerSize :: (Base b, Show b) => Int -> [b] -> String
prop_textKmerSize k text = test ++ sb ++ (show k) ++ "." ++ resm
  where
    test = "size of kmers obtained from  \"kmerOccs k text\""
    resm = if all (\c -> length c == k) (M.keys $ kmerOccs k text)
           then " Passed"
           else " Failed"

prop_textKmerOcc :: (Base b, Show b) => Int -> [b] -> String
prop_textKmerOcc k text = test ++ sb ++ (show k) ++ "." ++ resm
  where
    test = "kmer occurence obtained from \"kmerOccs k text\""
    resm = if all (\(u, xs) -> (all (\x -> u == take k (drop x text))) xs) (M.toList $ kmerOccs k text)
           then " Passed"
           else " Failed"
-}

intClumpEgs :: [([Int], [[Int]])]
intClumpEgs = [([1,2,3,4,11] , [[11], [4,3,2,1]])
              ,([1,2,3,4,5,12,13,14], [[14,13,12], [5,4,3,2], [4,3,2,1]])
              ]

egtest_intsClumps :: ([Int], [[Int]]) -> String
egtest_intsClumps t = test ++ (show x) ++ sb ++ (show y) ++ resm
  where
    x = fst t
    y = snd t
    res = clumpInts 3 1 x
    resm = if res == y
           then " Passed"
           else " Failed: Actual Value" ++ (show res)
    test = "clumpInts 3 1 "

clumerClumpEgs :: [(Clumer Char, [Clumer Char])]
clumerClumpEgs = map (
  \(xs, yss) -> (Clumer "AA" (set xs), map (\ys -> Clumer "AA" (set ys)) yss)
  ) intClumpEgs

egtest_baseSeqClumps :: (Base b, Show b) => (Clumer b, [Clumer b]) -> String
egtest_baseSeqClumps (x, y) = test ++ (show x) ++ sb ++ (show y) ++ resm
  where
    resm = if res == y
           then " Passed"
           else " Failed: Actual Value" ++ (show res)
    res  = clumps 3 1 x
    test = "clumps 3 1 "


egtest_clumpsRegCovSize :: (Base b, Show b) => (Clumer b, [Int]) -> String
egtest_clumpsRegCovSize (x, y) = test ++ (show x) ++ sb ++ (show y) ++ resm
  where
    resm = if res == y
           then " Passed"
           else " Failed: Actual Value" ++ (show res)
    res  = map regionCovered (clumps 3 1 x)
    test = "size of clumps 3 1 "

clumpSizeEgs :: [(Clumer Char, [Int])]
clumpSizeEgs = map (
  \(c, cs) -> (c, map regionCovered cs)
  ) clumerClumpEgs

prop_clumpsRegionIsSizeL :: (Base b, Show b) => Int -> Int -> Clumer b -> Bool
prop_clumpsRegionIsSizeL l t c = all (\r -> r <= (abs l) + 1) (map regionCovered cls)
  where cls = clumps (abs l) (1 + (abs t)) c

prop_clumpsSizeMinBound :: (Base b, Show b) => (Int, Int, Clumer b) -> Bool
prop_clumpsSizeMinBound (l, t, c) = all (\s -> s >= t) (map size cls)
  where cls = clumps l t c
  
 {-
prop_clumpParams :: (Base b, Show b) => Int -> Int -> Int -> Clumer b -> String
prop_clumpParams k l t c = test ++ (show c) ++ sb ++ y ++ "." ++ resm
  where
    resm  = if kpass && lpass && tpass
            then " Passed"
            else " Failed: Actual Value " ++ ska ++ ", " ++ sla ++ ", " ++ sta
    kpass = ka == k
    lpass = la >= l
    tpass = ta >= t
    ska   = show ka
    sla   = show la
    sta   = show ta
    ka    = length (element c)
    la    = maximum $ map regionCovered cls
    ta    = minimum $ map size cls
    cls   = clumps l t c
    y     = "k = " ++ (show k) ++ ", l >= " ++ (show l) ++ ", t >= " ++ (show t)
    test  = "clump parameters of "

prop_textClumps :: (Base b, Show b) => Int -> Int -> Int -> Clumer b -> String
prop_textClumps k l t text = test ++ sb ++ y ++ "." ++ resm
where
 test  = "parameters for clumps in kmers of text"
  resm  = if kpass && lpass && tpass
          then " Passed"
          else " Failed: Actual Value " ++ ska ++ ", " ++ sla ++ ", " ++ sta
-}
