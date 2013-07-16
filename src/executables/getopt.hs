module Main (
  main
) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import System (
    getArgs
  , getProgName
  )
import System.Console.GetOpt (
    getOpt
  , usageInfo
  , ArgDescr(NoArg, ReqArg, OptArg)
  , ArgOrder(RequireOrder)
  , OptDescr(Option)
  )

data Flag
  = Help
  | Version
  | Input String
  | Output String
  deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  case getOpt RequireOrder options args of
    ([], [], [])        -> doUsage
    ([Help], [], [])    -> doUsage
    ([Version], [], []) -> putStrLn "0.0.0"
    (flags, noOpts, []) -> do
      putStrLn "[flags]"
      putStrLn $ indentLines 2 flags
      putStrLn "[noOpts]"
      putStrLn $ indentLines 2 noOpts
    (_, _, msg)         -> error $ '\n' : intercalate "" msg

options :: [OptDescr Flag]
options = [
    Option "h" ["help"]
      (NoArg Help)
      "show help"
  , Option "vV" ["version"]
      (NoArg Version)
      "show version"
  , Option "i" ["input"]
      (ReqArg Input "FILE")
      "/path/to/input"
  , Option "o" ["output"]
      (OptArg (Output $ fromMaybe "stdout") "FILE")
      "/path/to/output"
  ]

doUsage :: IO ()
doUsage = do
  progName <- getProgName
  putStr $ usageInfo ("Usage: " ++ progName ++ " [OPTION...]") options

indentLines :: (Show a) => Int -> [a] -> String
indentLines n xs =
  intercalate "\n" (map (\x -> indent ++ show x) xs)
    where indent = replicate n ' '
