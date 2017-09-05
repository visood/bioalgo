{-# LANGUAGE GADTs #-}

module Genome.Dna.Variant where
import Data.Map (Map, (!))
import Data.Set.Monad (Set)
import qualified Data.Set.Monad as Set
import Genome.Dna.Dna
import Genome.Dna.Kmer
import Util.Util (hamdist)

type Mutation b = b -> b

mutateTo :: b -> Mutation b
mutateTo c = \_ -> c

mutateSinglePos :: Int -> Mutation b -> [b] -> [b]
mutateSinglePos pos mut seq = case (drop pos seq) of
                                []     -> beforePos
                                (x:ys) -> beforePos ++ ((mut x) : ys)
  where beforePos = take pos seq

mutateSeveralPos :: [(Int, Mutation b)] -> [b] -> [b]
mutateSeveralPos [] seq = seq
mutateSeveralPos ((p, m) : muts) seq = mutateSeveralPos muts mutseq
  where mutseq = mutateSinglePos p m seq
