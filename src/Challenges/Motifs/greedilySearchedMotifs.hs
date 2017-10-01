import Genome.Dna.Motif (greedilySearchedMotifs, rows)
import System.Environment (getArgs)
import Data.List.Split (splitWhen)

main = do
  args <- getArgs
  fileContent <- readFile (head args)
  let fileLines = lines fileContent
      kn        = splitWhen (==' ') $ head fileLines
      k         = read (head kn) :: Int
      n         = read (head $ tail kn) :: Int
      dna       = tail fileLines

  putStrLn $ "k: " ++ (show k)
  putStrLn $ "n: " ++ (show n)
  mapM_ putStrLn dna
  putStrLn "motifs found"
  mapM_ (\f -> putStr (f ++ "\n")) $ rows (greedilySearchedMotifs k dna)
  putStrLn ""
