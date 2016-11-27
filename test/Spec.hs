import Dna.Dna
import Dna.Kmer
import Test.Util.Util
import Test.Dna.Dna
import Test.Dna.Kmer
import Test.QuickCheck
import Test.HUnit
import Control.Monad
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
    pss <- return $ map egtest_intsClumps intClumpEgs
    putStrLn (concatStrList pss)
  putStrLn "-------------------------------------"
  do
    pss <- return $ map egtest_baseSeqClumps clumerClumpEgs
    putStrLn (concatStrList pss)
  putStrLn "-------------------------------------"
  do
    pss <- return $ map egtest_clumpsRegCovSize clumpSizeEgs
    putStrLn (concatStrList pss)
  do
    putStrLn ""
    putStr "test pattern is repeated: "
    quickCheckWith stdArgs {maxSuccess = 100000} (
      test_isRepeated :: Int -> [Nucleotide] -> [Nucleotide] -> Bool
      )
  do
    putStrLn ""
    putStr "test occurences: "
  quickCheckWith stdArgs {maxSuccess = 100000} (
    test_occurences :: [Nucleotide] -> [Int] -> Bool
    )
  do
    putStrLn ""
    putStr "test ptrn count: "
  quickCheckWith stdArgs {maxSuccess = 1000} (
    test_ptrnCount :: [Nucleotide] -> Bool
    )
  do
    putStrLn ""
<<<<<<< HEAD
    putStr "test next occurence of ptrn: "
  quickCheckWith stdArgs {maxSuccess = 100000} (
    test_nextOcc :: [Nucleotide] -> Int -> Bool
    )
  do
    putStrLn ""
    putStr "test prop_clumpsRegionIsSizeL: "
  quickCheckWith stdArgs {maxSuccess = 1000} (
    prop_clumpsRegionIsSizeL :: Int -> Int -> Clumer Nucleotide -> Bool
    )
  do
    putStrLn ""
    putStr "test prop_clumpsSizeMinBound: "
  quickCheckWith stdArgs {maxSuccess = 1000} (
    prop_clumpsSizeMinBound :: Int -> Int -> Clumer Nucleotide -> Bool
    )
  where
    concatStrList :: [String] -> String
    concatStrList = foldl (\x y -> x ++ "\n" ++ y) []
