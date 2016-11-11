import Dna

import Test.QuickCheck
import Test.HUnit
import Control.Monad
import Kmers
import qualified Data.Set as Set

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
  putStrLn "test that Int Base can be faithfully complemented"
  quickCheck (prop_idempotent_complement :: Int -> Bool)
  putStrLn "test that a sequence of Base Char can be faithfully reverse complemented"
  quickCheck (prop_idempotent_revcomp :: [Char] -> Bool)
  putStrLn "test that a sequence of Base Int can be faithfully reverse complemented"
  quickCheck (prop_idempotent_revcomp :: [Int] -> Bool)

  putStrLn "-------------------------------------"
  putStrLn "test that Ints are clumped correctly"
  do
    pss <- return $ map prop_intsCanBeClumped intClumpEgs
    putStrLn (concatStrList pss)
  putStrLn "-------------------------------------"
  do
    pss <- return $ map prop_baseSeqCanBeClumped clumerClumpEgs
    putStrLn (concatStrList pss)
  putStrLn "-------------------------------------"
  do
    pss <- return $ map prop_clumpsRegCovIsExpected clumpSizeEgs
    putStrLn (concatStrList pss)
  where
    concatStrList :: [String] -> String
    concatStrList = foldl (\x y -> x ++ "\n" ++ y) []

sb = " should be "

prop_idempotent_complement :: Base a => a -> Bool
prop_idempotent_complement c = complement (complement c) == c

prop_idempotent_revcomp    :: Base a => [a] -> Bool
prop_idempotent_revcomp cs = reverseComplement (reverseComplement cs) == cs

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


prop_clumpsRegCovIsExpected :: (Base b, Show b) => (Clumer b, [Int]) -> String
prop_clumpsRegCovIsExpected (x, y) = test ++ (show x) ++ sb ++ (show y) ++ resm
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

prop_clumpSatisfiesConstraints :: (Base b, Show b) => Int -> Int -> Int -> Clumer b -> String
prop_clumpSatisfiesConstraints k l t c = test ++ (show c) ++ sb ++ y ++ resm
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
    y     = "k = "(show k) ++ ", l >= " ++ (show l) ++ ", t >= " ++ (show t)
    test  = "clump parameters of "
