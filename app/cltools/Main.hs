module Main (main) where

import Control.Monad
import System.Environment(getArgs)
import System.Exit
import Data.Char
import Data.Map (Map, (!))
import qualified Data.Map as M
import Util.Command
import Genome.Dna.Kmer
import Genome.Dna.Dna

main :: IO ()
main = do
  args <- getArgs
  dispatch args

dispatch        :: [String] -> IO ()
dispatch ["-h"] = cltoolsUsage >> exit
dispatch ["-v"] = version >> exit
dispatch (u:("-h"):_) = putStrLn (usage u) >> exit
dispatch args = case command of
  Command "illegal" _ -> throwIllegalUtilityName
  Command "empty"   _ -> throwUnspecifiedUtility
  _                   -> if not(isAvailable command)
                         then throwUnavailableUtility
                         else if checkCommand command
                              then execute command
                              else throwIncompleteCommand command
  where command = readCommand args



version        = putStrLn "version 0.1"
exit           = exitWith ExitSuccess
die            = exitWith (ExitFailure 1)

