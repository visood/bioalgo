module Test.Genome.Dna.Dna where

import Test.Util.Util
import Genome.Dna.Dna

prop_idempotent_complement :: Base a => a -> Bool
prop_idempotent_complement c = complement (complement c) == c

prop_idempotent_revcomp    :: Base a => [a] -> Bool
prop_idempotent_revcomp cs = reverseComplement (reverseComplement cs) == cs
