import Kmers

import Test.QuickCheck

instance Arbitrary Char where
  arbitrary     = choose ('A', 'C', 'G', 'T', 'a', 'c', 'g', 't')
  coarbitrary c = variant (ord c `rem` 4)
