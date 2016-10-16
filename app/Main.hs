module Main (main) where

import Control.Monad
import System.Environment(getArgs)
import System.Exit
import Data.Char
import Data.Map (Map, (!))
import qualified Data.Map as M
import Lib

main :: IO ()
main = do
  args <- getArgs
  putStr "Arguments: "
  print args
  l <- parse args
  print l

tac = unlines . reverse . lines
parse ["-h"]     = usage >> exit
parse ["-v"]     = version >> exit
parse []         = getContents
parse args       = sayHelloJee (argMap ! "name")
                   where
                     argMap = parseArgs args

parseArgs        :: [String] -> Map String String
parseArgs        = M.fromList . (map parseOneArg)

parseOneArg      :: String -> (String, String)
parseOneArg s    = (dropWhile (=='-') u, dropWhile (=='=') v)
                   where (u, v) = break (=='=') s


usage            = putStrLn "Usage: greetings [-vh] [-name=<name>]"
version          = putStrLn "Haskell tac 0.1"
exit             = exitWith ExitSuccess
die              = exitWith (ExitFailure 1)

sayHelloJee      :: String -> IO String
sayHelloJee name = do
  putStr "Hello jee, "
  putStr name
  putStrLn ". sir jee, Hello."
  return ""


