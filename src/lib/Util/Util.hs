module Util.Util where

import System.Exit
import Data.Tuple
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Char
import Dna.Kmer
import Dna.Dna


data Command = Command {utility  :: String,
                        arguments :: Map String String
                       } deriving (Show)

astr :: Command -> String
astr command = foldl (
  \s xy -> s ++ " -" ++ (fst xy) ++ "=" ++ (snd xy) ++ " "
  ) (utility command)  (M.toList (arguments command) )

emptyCommand :: Command
emptyCommand = Command "empty-command" M.empty



readCommand :: [String] -> Command
readCommand [] = emptyCommand
readCommand (u:args) = if (wordWithAllowedChars u)
                       then Command u (readArgs args)
                       else Command "illegal-utility" M.empty

wordWithAllowedChars :: String -> Bool
wordWithAllowedChars []   = False
wordWithAllowedChars (x:xs) = (isAlphaNum x) && (all (\c -> (c == '-') || (isAlphaNum c)) xs)


readArgs :: [String] -> Map String String
readArgs = M.fromList . (map readOneArg)

readOneArg :: String -> (String, String)
readOneArg s = (dropWhile (=='-') u, dropWhile (=='=') v)
  where (u, v) = break (=='=') s


availableCommands :: Map String String
availableCommands = M.fromList [("pattern-count",
                                 "-f=<file-name> -p=<pattern>"),
                                ("most-frequent-kmers",
                                 "-f=<file-name> -k=<kmer-size> -n=<number>")
                               ]

usage :: String -> String
usage u = if M.notMember u availableCommands
          then "Exception: " ++ u ++ " is not a valid command."
          else "Usage: " ++ u ++ " " ++ (availableCommands ! u)


execute :: Command -> IO ()

execute (Command "hello" _) = do
  putStrLn "Hello jee! What may bioalgo tools do for you?"
  putStrLn "Please call me with a command"
  putStrLn ""
  putStrLn "!!! BYE for now !!!"

execute (Command "pattern-count" argMap) = do
  text <- readFile (argMap ! "f")
  pcounts <- return $ patternCount text (argMap ! "p")
  putStr "number of appearances of "
  putStr  (argMap ! "p")
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

execute (Command "reverse-complement" argMap) = do
  putStrLn (if (isDNA seq) then reverseComplement seq else "Not a DNA sequence")
  where seq = argMap ! "s"

execute  (Command "illegal-utility" _) = do
  putStrLn "Exception: illegal utility name."
  cltoolsUsage >> exitWith ExitSuccess

execute (Command "empty-command" _) = do
  putStrLn "Exception: utility not specified."
  cltoolsUsage >> exitWith ExitSuccess

execute (Command s _) = do
  putStrLn ("Exception: unknown utility " ++ s ++ ".")
  cltoolsUsage >> exitWith ExitSuccess

cltoolsUsage = do
  putStrLn "Usage: [-vh] utility arguments"
  putStrLn "Available utilities: "
  mapM (\ua -> putStrLn ((fst ua) ++ " " ++ (snd ua))) (M.toList availableCommands)

