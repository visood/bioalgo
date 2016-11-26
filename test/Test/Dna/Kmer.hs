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
prop_clumpsRegionIsSizeL x y c = all (\r -> r <= l + 1) (map regionCovered cls)
  where
    cls = clumps l t c
    l   = abs x
    t   = 1 + abs y

prop_clumpsSizeMinBound :: (Base b, Show b) => Int -> Int -> Clumer b -> Bool
prop_clumpsSizeMinBound x y c = all (\s -> s >= t) (map size cls)
  where
    cls = clumps l t c
    l   = abs x
    t   = 1 + abs y

test_patternCount ::  [Nucleotide] -> Bool
test_patternCount p1 = null p1 || patternCount p1 text == 100
  where
    text = concat $ take 100 (repeat $ p1 ++ [_N_])
