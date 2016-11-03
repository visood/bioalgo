import Dna

import Test.QuickCheck
import Test.HUnit
import Control.Monad

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

  putStrLn "test that Ints are clumped correctly"
  do
    pss <- return $ map prop_intsCanBeClumped1 clumpEgs
    putStrLn (foldl (\x y -> x ++ "\n" ++ y) [] pss)


prop_idempotent_complement :: Base a => a -> Bool
prop_idempotent_complement c = complement (complement c) == c

prop_idempotent_revcomp    :: Base a => [a] -> Bool
prop_idempotent_revcomp cs = reverseComplement (reverseComplement cs) == cs

clumpEgs :: [([Int], [[Int]])]
clumpEgs = [([1,2,3,4,11] , [[11], [4,3,2,1]])
           ,([1,2,3,4,5,12,13,14], [[14,13,12], [5,4,3,2], [4,3,2,1]])
           ]
prop_intsCanBeClumped :: [ ([Int], [[Int]])] -> [String]
prop_intsCanBeClumped egs = do
  (xs, ys) <- egs
  return (show xs)
  --putStrLn ((show (fst t)) ++ " --> " ++ (show (snd t)))
  --putStrLn (if (clumpInts 3 1 (fst t) == (snd t)) then "Passed" else "Failed")

prop_intsCanBeClumped1 :: ([Int], [[Int]]) -> String
prop_intsCanBeClumped1 t = test ++ (show x) ++ sb ++ (show y) ++ resm
  where
    x = fst t
    y = snd t
    res = clumpInts 3 1 x
    resm = if res == y
           then " Passed"
           else " Failed: Actual " ++ (show res)
    sb = " should be "
    test = "clumpInts 3 1 "
