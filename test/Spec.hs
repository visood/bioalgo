import Dna

import Test.QuickCheck
import Test.HUnit
import Control.Monad
import Kmers
import qualified Data.Set as Set
import qualified Data.Map as M
{- For now the tests are defined here, along with the main.
Tests in general should fall in the same folder structure as the main library,
locationOfTest testName = test/dir/file.hs
where
  dir = directoryContaining testName in src/
  file.hs = fileContaining testName in src/dir/
-}
main :: IO ()
main = do
  putStrLn " tests"
  putStrLn "test that Char Base can be faithfully complemented"
  quickCheck (prop_idempotent_complement :: Char -> Bool)
  putStrLn "that Int Base can be faithfully complemented"
  quickCheck (prop_idempotent_complement :: Int -> Bool)
  putStrLn "that a sequence of Base Char can be faithfully reverse complemented"
  quickCheck (prop_idempotent_revcomp :: [Char] -> Bool)
  putStrLn "that a sequence of Base Int can be faithfully reverse complemented"
  quickCheck (prop_idempotent_revcomp :: [Int] -> Bool)

  putStrLn "-------------------------------------"
  putStrLn "that Ints are clumped correctly"
  do
    pss <- return $ map prop_intsCanBeClumped intClumpEgs
    putStrLn (concatStrList pss)
  putStrLn "-------------------------------------"
  do
    pss <- return $ map prop_baseSeqCanBeClumped clumerClumpEgs
    putStrLn (concatStrList pss)
  putStrLn "-------------------------------------"
  do
    pss <- return $ map prop_clumpsRegCovSize clumpSizeEgs
    putStrLn (concatStrList pss)
  where
    concatStrList :: [String] -> String
    concatStrList = foldl (\x y -> x ++ "\n" ++ y) []

sb = " should be "

prop_idempotent_complement :: Base a => a -> Bool
prop_idempotent_complement c = complement (complement c) == c

prop_idempotent_revcomp    :: Base a => [a] -> Bool
prop_idempotent_revcomp cs = reverseComplement (reverseComplement cs) == cs

prop_textKmerSize :: (Base b, Show b) => Int -> [b] -> String
prop_textKmerSize k text = test ++ sb ++ (show k) ++ "." ++ resm
  where
    test = "size of kmers obtained from  \"kmerOccs k text\""
    resm = if all (\c -> length c == k) (M.keys $ kmerOccs k text)
           then " Passed"
           else " Failed"

intClumpEgs :: [([Int], [[Int]])]
intClumpEgs = [([1,2,3,4,11] , [[11], [4,3,2,1]])
              ,([1,2,3,4,5,12,13,14], [[14,13,12], [5,4,3,2], [4,3,2,1]])
              ]

prop_intsCanBeClumped :: ([Int], [[Int]]) -> String
prop_intsCanBeClumped t = test ++ (show x) ++ sb ++ (show y) ++ resm
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

prop_baseSeqCanBeClumped :: (Base b, Show b) => (Clumer b, [Clumer b]) -> String
prop_baseSeqCanBeClumped (x, y) = test ++ (show x) ++ sb ++ (show y) ++ resm
  where
    resm = if res == y
           then " Passed"
           else " Failed: Actual Value" ++ (show res)
    res  = clumps 3 1 x
    test = "clumps 3 1 "


prop_clumpsRegCovSize :: (Base b, Show b) => (Clumer b, [Int]) -> String
prop_clumpsRegCovSize (x, y) = test ++ (show x) ++ sb ++ (show y) ++ resm
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

--prop_textClumps :: (Base b, Show b) => Int -> Int -> Int -> Clumer b -> String
--prop_textClumps k l t text = test ++ sb ++ y ++ "." ++ resm
--where
 -- test  = "parameters for clumps in kmers of text"
 -- resm  = if kpass && lpass && tpass
  --        then " Passed"
   --       else " Failed: Actual Value " ++ ska ++ ", " ++ sla ++ ", " ++ sta
