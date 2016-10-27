module Dna where

complement :: Char -> Char
complement 'A' = 'T'
complement 'a' = 't'
complement 'C' = 'G'
complement 'c' = 'g'
complement 'G' = 'C'
complement 'g' = 'c'
complement 'T' = 'A'
complement 't' = 'a'
complement 'N' = 'N'
complement 'n' = 'n'
complement  b  =  b

reverseComplement :: String -> String

reverseComplement = foldl (\rs x -> (complement x):rs) []

isBase :: Char -> Bool
isBase c = c == 'A' || c == 'C' || c == 'G' || c == 'T'

isDNA :: String -> Bool
isDNA seq = all isBase  seq

