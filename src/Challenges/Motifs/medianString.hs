import Genome.Dna.Motif (medianString)
import System.Environment (getArgs)

main = do
  args <- getArgs
  fileContent <- readFile (head args)
  let fileLines = lines fileContent
      k         = read (head fileLines) :: Int
      dna       = tail fileLines

  putStrLn $ "k: " ++ (show k)
  putStrLn "dna"
  mapM_ putStrLn dna
  putStrLn "Median String"
  putStrLn $ medianString k dna
