-- @Milos Prokop, 2024, free to use and modify

import Quipper
import Quipper.Libraries.Arith

import Quipper.Utils.Auxiliary
import Quipper.Libraries.QFT
import Quipper.Utils.CommandLine
import Quipper.Utils.Sampling
import Quipper.Libraries.Decompose
import Quipper.Internal.Printing

import Quipper.Utils.Auxiliary
import Quipper.Libraries.ClassicalOptim.QuipperInterface
import Control.Monad (foldM, zipWithM, replicateM)

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Random

import SvpFunctions

-- | A data type to hold values set by command line options.
data Options = Options {
  n :: Int                    -- ^Parameter /n/ (1=testAdderCircuit).
} deriving Show

-- | The default options.
defaultOptions :: Options
defaultOptions = Options
  {
    n = 1
  }

-- | The list of command line options, in the format required by 'getOpt'.
options :: [OptDescr (Options -> IO Options)]
options =
  [ 
-- Generic options
    Option ['h'] ["help"]      (NoArg help)                    "print usage info and exit",
-- Algorithm parameter 
    Option ['n'] ["n"]         (ReqArg nnn "<n>")              "parameter n (default: 2)"
  ]
    where
      help :: Options -> IO Options
      help o = do
        usage
        exitSuccess
    
      nnn :: String -> Options -> IO Options
      nnn string o =
        case parse_int string of 
          Just n | n >= 1 -> return o { n = n }
          _ -> optfail ("Invalid value for parameter n -- " ++ string ++ "\n")

-- | Process /argv/-style command line options into an 'Options' structure.
dopts :: [String] -> IO Options
dopts argv =
  case getOpt Permute options argv of
    (o, [], []) -> (foldM (flip id) defaultOptions o)
    (_, _, []) -> optfail "Too many non-option arguments\n"
    (_, _, errs) -> optfail (concat errs)

-- | Print usage message to 'stdout'.
usage :: IO ()
usage = do
  putStr (usageInfo header options) 
    where header = "Usage: test [OPTION...]"

main :: IO()
main = do
    --print $ "Test_all: " ++ show test_all
    argv <- getArgs
    options <- dopts argv 
    case options of
        Options { n = n} ->
           if n == 1
            then do
                print_generic ASCII $ decompose_generic Logical testAdderCircuit 
           else if n == 2
            then do
                print_generic ASCII $ decompose_generic Logical testMultiplicationCircuit
           else if n == 3
            then do
                print_generic ASCII $ decompose_generic Logical (testSubtractionCircuit 0)
           else if n == 4
            then do
                print_generic ASCII $ decompose_generic Logical (testSubtractionCircuit 1)
           else if n == 5
            then do
                print_generic ASCII $ decompose_generic Logical (testSubtractionCircuit 2)
           else if n == 6
            then do
                print_generic ASCII $ decompose_generic Logical (testSubtractionCircuit 3)
           else return ()
           
