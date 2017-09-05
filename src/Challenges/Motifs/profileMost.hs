import Genome.Dna.Motif (profileMatrixFromRows, mostProbableMotif)
import System.Environment (getArgs)
import Data.List.Split (splitWhen)

main = do
  args <- getArgs
  fileContent <- readFile (head args)
  let fileLines = lines fileContent
      text      = head fileLines
      k         = read (head $ tail fileLines) :: Int
      pmrowStrs = tail $ tail fileLines
      pmStrs    = map (\rowStr -> splitWhen (==' ') rowStr) pmrowStrs
      pm        = map (map (\s -> read s :: Double)) pmStrs

  putStrLn ("k: " ++ (show k))
  mapM_ (\row -> putStrLn (foldl (\s x -> s ++ ", " ++ (show x)) "" $ row)) pm

  putStrLn "most probable motif in text "
  putStrLn (mostProbableMotif k (profileMatrixFromRows pm) text)
