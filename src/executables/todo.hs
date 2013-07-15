module Main where

import Control.Exception (bracketOnError)
import System (getArgs, getProgName)
import System.Directory (removeFile, renameFile)
import System.IO (hClose, hPutStr, openTempFile)

main = do
  args <- getArgs
  doCommand args

doCommand :: [String] -> IO ()
doCommand ("create":args)  = doCreate args
doCommand ("read":args)    = doRead   args
doCommand ("update":args)  = doUpdate args
doCommand ("delete":args)  = doDelete args
doCommand _                = doUsage

doUsage :: IO ()
doUsage = do
  progName <- getProgName
  putStrLn ("Usage: " ++ progName ++ " (create|read|update|delete) <filename> <args, ...>")

doCreate :: [String] -> IO ()
doCreate [fn, ln] = do
  appendFile fn (ln ++ "\n")
  doRead [fn]
doCreate _        = doUsage

doRead :: [String] -> IO ()
doRead [fn] = do
  contents <- readFile fn
  let vs = zipWith (\n ln -> show n ++ " - " ++ ln) [0..] (lines contents)
  putStr $ unlines vs
doRead _ = doUsage

doUpdate :: [String] -> IO ()
doUpdate [fn, n, ln] = do
  contents <- readFile fn
  let v = unlines $ zipWith (\x y -> if show x == n then ln else y) [0..] (lines contents)
  updateFile' fn v
  doRead [fn]
doUpdate _ = doUsage

doDelete :: [String] -> IO ()
doDelete [fn, n] = do
  contents <- readFile fn
  let ts = zip [0..] (lines contents)
  let v = unlines $ snd $ unzip $ filter (\(i, ln) -> show i /= n) ts
  updateFile' fn v
  doRead [fn]
doDelete _ = doUsage

updateFile' :: String -> String -> IO ()
updateFile' fn v =
  bracketOnError (openTempFile "/tmp" "todo.txt")
    (\(tn, th) -> do
      hClose th
      removeFile tn)
    (\(tn, th) -> do
      hPutStr th v
      hClose th
      removeFile fn
      renameFile tn fn)
