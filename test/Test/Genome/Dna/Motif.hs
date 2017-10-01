module Test.Genome.Dna.Motif where

import Test.Util.Util
import Genome.Dna.Dna
import Genome.Dna.Motif

import Test.QuickCheck (Gen, choose, elements, generate,
                        listOf, listOf1, vectorOf)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
