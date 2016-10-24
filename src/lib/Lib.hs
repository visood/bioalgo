module Lib where

import Data.Tuple
import Data.Map (Map, (!))
import qualified Data.Map as M
import Kmers

data Command = Command {utility  :: String,
                        arguments :: Map String String
                       } deriving (Show)

astr :: Command -> String
astr command = foldl (
  \s xy -> s ++ " -" ++ (fst xy) ++ "=" ++ (snd xy) ++ " "
  ) (utility command)  (M.toList (arguments command) )

emptyCommand :: Command
emptyCommand = Command "do-nothing" M.empty

readCommand :: [String] -> Command
readCommand [] = emptyCommand
readCommand (u:args) = Command u (readArgs args)

readArgs :: [String] -> Map String String
readArgs = M.fromList . (map readOneArg)

readOneArg :: String -> (String, String)
readOneArg s = (dropWhile (=='-') u, dropWhile (=='=') v)
  where (u, v) = break (=='=') s


execute :: Command -> IO ()

execute (Command "hello" _) = do
  putStrLn "Hello jee! What may bioalgo tools do for you?"
  putStrLn "Please call me with a command"
  putStrLn "!!! BYE for now !!!"

execute (Command "pattern-count" argMap) = do
  text <- readFile fname
  pcounts <- return $ patternCount text pattern
  putStr "number of appearances of "
  putStr  pattern
  putStr ": "
  putStrLn (show pcounts)
  where
    fname   = argMap ! "f"
    pattern = argMap ! "p"

execute (Command "most-frequent-kmers" argMap) = do
  text <- readFile f
  kcounts <- return $ kmerCounts text k
  putStr (show (sum kcounts))
  putStr (" total ")
  putStr (show k)
  putStrLn "-mers found."
  putStr " of which "
  putStr (show (length kcounts))
  putStrLn " are unique."
  putStr "Top "
  putStr (show n)
  putStr (" ")
  putStr (show k)
  putStrLn "-mers: "
  mapM_ (\(s, n) -> putStrLn ( (show n) ++ ": " ++ s)) (
    topFrequentKmers n kcounts
    )
  where
    k = read (argMap ! "k") :: Int
    n = read (argMap ! "n") :: Int
    f = argMap ! "f"

execute (Command s _) = do
  putStr "unknown command "
  putStrLn s

