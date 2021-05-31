module Main where

import System.Environment
import System.IO
import System.Exit

import Parser

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    [] -> getContents
    [fileName] -> readFile fileName
    _ -> hPutStrLn stderr "Too many arguments given" >> exitFailure
  case parse parseIni input of
    Just i -> print i
    Nothing -> do
      hPutStrLn stderr "Failed to parse INI file."
      exitFailure
