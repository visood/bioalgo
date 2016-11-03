import Dna

import Test.QuickCheck
import Test.HUnit

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



prop_idempotent_complement :: Base a => a -> Bool
prop_idempotent_complement c = complement (complement c) == c

prop_idempotent_revcomp    :: Base a => [a] -> Bool
prop_idempotent_revcomp cs = reverseComplement (reverseComplement cs) == cs
