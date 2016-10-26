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
  dispatch args

dispatch        :: [String] -> IO ()
dispatch ["-h"] = cltoolsUsage >> exit
dispatch ["-v"] = version >> exit
dispatch (u:("-h"):_) = putStrLn (usage u) >> exit
dispatch []     = getContents >> exit
dispatch args   = execute (readCommand args)



cltoolsUsage = do
  putStrLn "Usage: [-vh] utility arguments"
  putStrLn "Available utilities: "
  mapM (\ua -> putStrLn ((fst ua) ++ " " ++ (snd ua))) (M.toList availableCommands)

version        = putStrLn "version 0.1"
exit           = exitWith ExitSuccess
die            = exitWith (ExitFailure 1)

