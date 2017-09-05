import Util.Util (hamdist)
import System.Environment(getArgs)
 
main = do
  args <- getArgs
  fileContent <- readFile (head args)
  let seqs = lines fileContent
  putStrLn (show (hamdist (head seqs) (head $ tail seqs)))

