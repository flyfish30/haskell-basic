module Main where

import System.Exit (ExitCode(..), exitFailure, exitSuccess, exitWith)
import FreeX
import FixedVector
import ComonadImg

main :: IO ()
main = do
  putStrLn "Hello world!"
  exitSuccess
