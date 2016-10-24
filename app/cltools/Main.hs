module Main (main) where

import Control.Monad
import System.Environment(getArgs)
import System.Exit
import Data.Char
import Data.Map (Map, (!))
import qualified Data.Map as M
import Lib
import Kmers

main :: IO ()
main = do
  args <- getArgs
  command <- return $ readCommand args
  putStr "Command: "
  print (astr command)
  execute command

dispatch        :: [String] -> IO ()
dispatch ["-h"] = usage >> exit
dispatch ["-v"] = version >> exit
dispatch []     = getContents >> exit
dispatch args   = execute (readCommand args)

usage          = putStrLn "Usage: [-vh] [-k=<kmer-size> -n=<top-kmers> -f=<sequence-file]"
version        = putStrLn "version 0.1"
exit           = exitWith ExitSuccess
die            = exitWith (ExitFailure 1)

