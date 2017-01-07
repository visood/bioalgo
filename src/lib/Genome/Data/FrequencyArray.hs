{-#LANGUAGE GADTs, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Genome.Data.FrequencyArray where

import Data.List (sort)
import Data.Set (Set)
import Data.Sequence (Seq)
import Data.Sequence ((><), (<|), (|>))
import qualified Data.Foldable as Foldable

