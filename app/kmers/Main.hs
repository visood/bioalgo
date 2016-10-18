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
  putStr "Arguments: "
  print args
  l <- parse args
  print l

parse ["-h"]     = usage >> exit
parse ["-v"]     = version >> exit
parse []         = getContents
parse args       = doKmerCount k n f
                   where
                     k = read (argMap ! "k") :: Int
                     n = read (argMap ! "n") :: Int
                     f = argMap ! "f"
                     argMap = parseArgs args

parseArgs        :: [String] -> Map String String
parseArgs        = M.fromList . (map parseOneArg)

parseOneArg      :: String -> (String, String)
parseOneArg s    = (dropWhile (=='-') u, dropWhile (=='=') v)
                   where (u, v) = break (=='=') s


usage            = putStrLn "Usage: [-vh] [-k=<kmer-size> -n=<top-kmers> -f=<sequence-file]"
version          = putStrLn "version 0.1"
exit             = exitWith ExitSuccess
die              = exitWith (ExitFailure 1)

doKmerCount     :: Int -> Int -> String -> IO String
doKmerCount k n fname = do
  text <- readFile fname
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
  return " "



