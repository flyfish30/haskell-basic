module Main where

import System.Exit (ExitCode(..), exitFailure, exitSuccess, exitWith)

main :: IO ()
main = do
  putStrLn "Hello world!"
  exitSuccess
