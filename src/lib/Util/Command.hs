module Util.Command where

import System.Exit
import Data.Tuple
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Char
import Genome.Dna.Kmer
import Genome.Dna.Dna

data Command = Command {utility   :: String,
                        arguments :: Map String String
                       }
instance Show Command where
  show (Command name args) = foldl addArg ("command" ++ name) (M.toList args)
    where addArg = \s xy -> s ++ " -" ++ (fst xy) ++ "=" ++ (snd xy)


emptyCommand = Command "empty" M.empty

readCommand :: [String] -> Command
readCommand []        = emptyCommand
readCommand (u:args)  = if (allowedWord u)
                        then Command u (readArgs args)
                        else Command "illegal" M.empty
  where
    allowedWord :: String -> Bool
    allowedWord [] = False
    allowedWord (x:xs) = (isAlphaNum x) && all allowedChar xs
    allowedChar = \c -> (c == '-') || (isAlphaNum c)

readArgs :: [String] -> Map String String
readArgs = M.fromList . (map readOneArg)
  where
    readOneArg s = (tail $ takeWhile (/= '=') s, tail $ dropWhile (/= '=') s)

availableCommands = M.fromList [
  ("ptrn-count", "-f=<file-name> -p=<ptrn>"),
  ("most-frequent-kmers", "-f=<file-name> -k=<kmer-size> -n=<number>")
  ]

usage u = if M.notMember u availableCommands
          then "Exception: " ++ u ++ " is not a valid command."
          else "Usage: " ++ u ++ " " ++ (availableCommands ! u)

execute :: Command -> IO()
execute (Command "hello" _) = do
  putStrLn "Hello jee! What may bioalgo tool-kit do for you?"
  putStrLn "Please call me with a command"
  putStrLn ""
  putStrLn "!!! BYE for now !!!"

execute (Command "ptrn-count" argMap) = do
  text <- readFile (argMap ! "f")
  pcounts <- return $ ptrnCount (argMap ! "p") text 
  putStr "number of appearances of "
  putStr (argMap ! "p")
  putStr ": "
  putStrLn (show pcounts)

execute (Command "most-frequent-kmers" argMap) = do
  text <- readFile f
  kcounts <- return $ kmerCounts k text
  putStr (show (sum kcounts))
  putStr (" total ")
  putStr (show k)
  putStrLn "-mers found."
  putStr " of which "
  putStr (show (length kcounts))
  putStrLn " are unique." 
  putStrLn $ "Top " ++ (show n) ++ " " ++ (show k) ++ "-mers: "
  mapM_ (\(s, m) -> putStrLn ((show m) ++ ": " ++ s))(topFrequentKmers n kcounts)
  where
    k = read (argMap ! "k") :: Int
    n = read (argMap ! "n") :: Int
    f = argMap ! "f"

execute (Command "reverse-complement" argMap) = do
  putStrLn (if (isDNA seq) then reverseComplement seq else "Not a DNA sequence")
  where seq = argMap ! "s"

execute (Command "illegal" _) = do
  putStrLn "Exception: illegal utility name."
  cltoolsUsage >> exitWith ExitSuccess

execute (Command "empty" _) = do
  putStrLn "Exception: utility not specified."
  cltoolsUsage >> exitWith ExitSuccess

-- last case, when every other case unmet
execute (Command s _) = do
  putStrLn $ "Exception: unknown utility " ++ s ++ "."
  cltoolsUsage >> exitWith ExitSuccess

cltoolsUsage = do
  putStrLn "Usage: [-vh] <utility> <arguments>"
  putStrLn "Available utilities: "
  mapM pcmd (M.toList availableCommands)
    where pcmd = (\ua -> putStrLn ((fst ua) ++ " " ++ (snd ua)))
