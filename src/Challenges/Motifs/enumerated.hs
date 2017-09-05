import Genome.Dna.Motif (isMotif, enumeratedMotifs)
import Data.Map (toList)
import System.Environment (getArgs)
import Data.List.Split (splitWhen)

main = do
  args <- getArgs
  fileContent <- readFile (head args)
  let fileLines = lines fileContent
      kd        = splitWhen (==' ') $ head fileLines
      k         = read (head kd) :: Int
      d         = read (head $ tail kd) :: Int
      dna       = tail fileLines

  putStrLn $ "k: " ++ (show k)
  putStrLn $ "d: " ++ (show d)
  mapM_ putStrLn dna
  putStrLn "motifs found"
  mapM_ (\f -> putStr (f ++ " ")) $ enumeratedMotifs k d dna
  putStrLn ""

