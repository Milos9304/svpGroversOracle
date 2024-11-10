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
  n :: Int,                    -- ^Parameter /n/ (lattice 1st dimension).
  m :: Int,                    -- ^Parameter /m/ (lattice 2nd dimension).
  b :: Int,              -- ^Parameter /b/ (coefficient bounds).
  format :: Format,            -- ^The output format.
  gatebase :: GateBase         -- ^What kind of gates to decompose into.
} deriving Show

-- | The default options.
defaultOptions :: Options
defaultOptions = Options
  { n = 2,
    m = 2,
    b = 2,
    format = ASCII,
    gatebase = Logical
  }

-- | The list of command line options, in the format required by 'getOpt'.
options :: [OptDescr (Options -> IO Options)]
options =
  [ 
-- Generic options
    Option ['h'] ["help"]      (NoArg help)                    "print usage info and exit",
    Option ['f'] ["format"]    (ReqArg format "<format>")      "output format for circuits (default: ascii)",
    Option ['g'] ["gatebase"]  (ReqArg gatebase "<gatebase>")  "type of gates to decompose into (default: logical)",
-- Algorithm parameter 
    Option ['n'] ["n"]         (ReqArg nnn "<n>")              "parameter n (default: 2)",
    Option ['m'] ["m"]         (ReqArg mmm "<m>")              "parameter m (default: 2)",
    Option ['b'] ["b"]         (ReqArg bbb "<b>")              "parameter b (default: [4,4]) = n-length list of bounds"
  ]
    where
      help :: Options -> IO Options
      help o = do
        usage
        exitSuccess
    
      format :: String -> Options -> IO Options
      format str o = do
        case match_enum format_enum str of
          [(_, f)] -> return o { format = f }
          [] -> optfail ("Unknown format -- " ++ str ++ "\n")
          _ -> optfail ("Ambiguous format -- " ++ str ++ "\n")

      gatebase :: String -> Options -> IO Options
      gatebase str o = do
        case match_enum gatebase_enum str of
          [(_, f)] -> return o { gatebase = f }
          [] -> optfail ("Unknown gate base -- " ++ str ++ "\n")
          _ -> optfail ("Ambiguous gate base -- " ++ str ++ "\n")

      nnn :: String -> Options -> IO Options
      nnn string o =
        case parse_int string of 
          Just n | n >= 1 -> return o { n = n }
          _ -> optfail ("Invalid value for parameter n -- " ++ string ++ "\n")

      mmm :: String -> Options -> IO Options
      mmm string o =
        case parse_int string of
          Just m | m >= 1 -> return o { m = m }
          _ -> optfail ("Invalid value for parameter n -- " ++ string ++ "\n")

      bbb :: String -> Options -> IO Options
      bbb string o =
        case parse_int string of 
          Just b -> return o { b = b }
          _ -> optfail ("Invalid value for parameter b -- " ++ string ++ "\n")

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
  putStr (show_enum "format" format_enum)
  putStr (show_enum "gatebase" gatebase_enum)
    where header = "Usage: svp [OPTION...]"

main :: IO()
main = do
    --print $ "Test_all: " ++ show test_all
    argv <- getArgs
    options <- dopts argv 
    case options of
        Options { format = format, gatebase = gatebase, n = n, m=m, b = b} ->
            
            --in PAPER
            -- UNCOMMENT FOR LOG N QUBITS PER DIMENSION
            print_generic format $ decompose_generic gatebase (svpOracle n m (ceiling(logBase 2 (fromIntegral n)))) 
            
            -- UNCOMMENT FOR b QUBITS PER DIMENSION
            -- print_generic format $ decompose_generic gatebase (svpOracle n m b)
            --
