import Genome.Dna.Kmer (mostFreqMismatchingKmers)
import System.Environment (getArgs)
import Data.List.Split (splitWhen)

main = do
  args <- getArgs
  fileContent <- readFile (head args)
  let fileLines = lines fileContent
      text      = head fileLines
      kd        = splitWhen (==' ') $ head (tail fileLines)
      k         = read (head kd) :: Int
      d         = read (head $ tail kd) :: Int
      mfq       = snd $ mostFreqMismatchingKmers d k text
  putStrLn ("inputs: " ++ text ++ " " ++ (show d) ++ " " ++ (show k))
  mapM (\x -> putStr (x ++ " ") ) mfq
  putStrLn ""
