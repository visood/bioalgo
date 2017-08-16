module Util.Command where

import System.Exit
import Data.Tuple
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Char
import Genome.Dna.Kmer
import Genome.Dna.Dna

data Command = Command {utility   :: String,
                        arguments :: Map String String}

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
    allowedWord (x:xs) = (isAlpha x) && all allowedChar xs
    allowedChar = \c -> (c == '-') || (isAlphaNum c)

readArgs :: [String] -> Map String String
readArgs = M.fromList . (map readOneArg)
  where
    readOneArg s = (tail $ takeWhile (/= '=') s, tail $ dropWhile (/= '=') s)

availableCommands :: Map String [String]
availableCommands = M.fromList [
  ("pattern-count", ["-f=<file-name>",
                     "-p=<ptrn>"]),
  ("most-frequent-kmers", ["-f=<file-name>",
                           "-k=<kmer-size>",
                           "-n=<rank>"]),
  ("reverse-complement", ["-f=<file-name>"]),
  ("occurences", ["-s=<pattern>",
                  "-f=<file-name>"]),
  ("clumps", ["-k=<kmer-length>",
              "-l=<window-length>",
              "-t=<number-clusters>",
              "-f=<genome-file-name",
              "-o=<output-count-or-patterns>"])
  ]



isAvailable :: Command -> Bool
isAvailable (Command command _) = M.member command availableCommands

checkCommand :: Command -> Bool
checkCommand (Command command argMap) = if M.member command availableCommands
                                        then all checkField requiredArgs
                                        else False
  where
    requiredArgs = M.toList (readArgs (availableCommands ! command))
    checkField   = \field -> M.member (fst field) argMap


throwIllegalUtilityName :: IO()
throwIllegalUtilityName = do
  putStrLn "Illegal utility name"
  putStrLn "--------------------------"
  cltoolsUsage >> exitWith ExitSuccess

throwUnspecifiedUtility :: IO()
throwUnspecifiedUtility = do
  putStrLn "Unspecified utility name."
  putStrLn "--------------------------"
  cltoolsUsage >> exitWith ExitSuccess

throwUnavailableUtility :: IO()
throwUnavailableUtility = do
  putStrLn "Specified utility is not available."
  putStrLn "--------------------------"
  cltoolsUsage >> exitWith ExitSuccess
  
throwIncompleteCommand :: Command -> IO()
throwIncompleteCommand (Command command argMap) = do
  putStrLn "Provide all the required arguments."
  putStrLn "--------------------------"
  putStrLn (usage command)

execute :: Command -> IO()
execute (Command "hello" _) = do
  putStrLn "Hello jee! What may bioalgo tool-kit do for you?"
  putStrLn "Please call me with a command"
  putStrLn ""
  putStrLn "!!! BYE for now !!!"

execute (Command "pattern-count" argMap) = do
  text <- readFile (argMap ! "f")
  pcounts <- return $ patternCount (argMap ! "p") text 
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
  text <- readFile filename
  let seq = head $ lines text -- assuming only single line file
  putStrLn (if (isDNA seq) then reverseComplement seq else "Not a DNA sequence")
  where
    filename = argMap ! "f"

execute (Command "occurences" argMap) = do
  textn <- readFile filename
  let text = head $ lines textn -- assuming only single line file
  mapM_ (\c -> putStr ((show c) ++ " ")) (occurences seq text)
  putStrLn ""
  where
    filename = argMap ! "f"
    seq      = argMap ! "s"

execute (Command "clumps" argMap) = do
  textn <- readFile f
  let text          = head $ lines textn -- assuming only single line file
      kmerClumpList = M.toList (kmerClumps k l t text)
      count         = length kmerClumpList
      showKmer      = \(kmer, _) -> putStr (kmer ++ " ")
  if (o == "count")
    then (putStr $ show count)
    else mapM_ showKmer kmerClumpList
  putStrLn ""
  where
    k = read (argMap ! "k") :: Int
    l = read (argMap ! "l") :: Int
    t = read (argMap ! "t") :: Int
    f = argMap ! "f"
    o = if (M.member "o" argMap)
        then argMap ! "o"
        else "patterns"
    
  
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
    where
      pcmd = (\c -> putStrLn ((fst c) ++ " " ++
                              (foldl (\o s -> o ++ "\n\t" ++ s) "" (snd c))))

usage u = if M.notMember u availableCommands
          then "Exception: " ++ u ++ " is not a valid command."
          else "Usage: " ++ u ++ " " ++ (showOptions u)
  where
    showOptions :: String -> String
    showOptions u = foldl (\o s -> (o ++ " " ++ s)) [] (availableCommands ! u)

