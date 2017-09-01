import Genome.Dna.Kmer (withinHammingDistance)
import Data.Map (toList)
import System.Environment(getArgs)

main = do
  args <- getArgs
  fileContent <- readFile (head args)
  let fileLines = lines fileContent
      word  = head fileLines
      text  = head (tail fileLines)
      d     = read (head $ tail $ tail fileLines) :: Int
      k     = length word
      --appx  = (\(kmer, _) -> hamdist kmer word <= d)
      --koccs = filter appx $ toList (kmerOccs k text)
      koccs = toList $ withinHammingDistance d word text
      appxKoccs = foldl (\occs (_, xs) -> occs ++ xs) [] koccs
  mapM (\x -> putStr ((show x) ++ " ")) appxKoccs
  putStrLn ("count: " ++ (show $ length appxKoccs))
